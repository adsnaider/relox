mod combine;

use std::fmt::Display;

use combine::{any, opt, Parse, StructuredLexer};
use miette::Diagnostic;
use thiserror::Error;

use crate::lox::ast::BinaryOp;

use super::{
    ast::{
        span::{Span, Spanned},
        Expr, Lit, LoxAst, Num, PrefixOp, Stmt, VarDecl,
    },
    lexer::{LexError, Lexeme, Lexer, Token, TokenValue, TokenVariants},
};

#[derive(Debug)]
pub struct Parser {}

#[derive(Debug, Error, Diagnostic)]
pub enum ParserError<'a> {
    #[error("Failure during lexing")]
    LexerError(#[related] Vec<LexError<'a>>),
    #[error(transparent)]
    #[diagnostic(transparent)]
    UnexpectedEof(UnexpectedEof<'a>),
    #[error(transparent)]
    #[diagnostic(transparent)]
    UnexpectedToken(UnexpectedToken<'a>),
    #[error("Failed assertion")]
    #[diagnostic(code(parser::fail))]
    Fail,
}

impl<'a> ParserError<'a> {
    pub fn unexpected_token(token: Token<'a>) -> Self {
        Self::UnexpectedToken(UnexpectedToken { token })
    }

    pub fn unexpected_eof(wanted: Option<TokenVariants>, source: &'a str) -> Self {
        let start = if source.len() == 0 {
            0
        } else {
            source.len() - 1
        };
        Self::UnexpectedEof(UnexpectedEof {
            wanted,
            source_code: source,
            end_of_file: Span::new(start, source.len()),
        })
    }
}

#[derive(Debug, Error, Diagnostic)]
#[error("Unexpected token: {}", .token.lexeme)]
#[diagnostic(code(parser::unexpected_token))]
pub struct UnexpectedToken<'a> {
    #[label("Here")]
    #[source_code]
    token: Token<'a>,
}

#[derive(Debug, derive_more::Error, Diagnostic)]
#[diagnostic(code(parser::unexpected_eof))]
pub struct UnexpectedEof<'a> {
    pub wanted: Option<TokenVariants>,
    pub source_code: &'a str,
    #[label("Here")]
    end_of_file: Span,
}

impl Display for UnexpectedEof<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(wanted) = self.wanted.as_ref() {
            write!(f, "Unexpected end of file - Wanted: {}", wanted)
        } else {
            write!(f, "Unexpected end of file")
        }
    }
}
pub type PResult<'a, T> = Result<T, ParserError<'a>>;

impl Parser {
    fn new() -> Self {
        Self {}
    }

    pub fn parse<'a>(lexer: Lexer<'a>) -> PResult<'a, Spanned<LoxAst>> {
        let mut this = Self::new();
        this.ast(&mut StructuredLexer::new(lexer))
    }

    pub fn ast<'a>(&mut self, lexer: &mut StructuredLexer<'a>) -> PResult<'a, Spanned<LoxAst>> {
        log::debug!("Parsing the AST");
        let span = lexer.enter_span();
        let mut stmts = Vec::new();
        while let Some(stmt) = opt(|l: &mut _| self.parse_declaration(l)).parse_next(lexer)? {
            stmts.push(stmt);
        }
        let span = span.end(lexer);
        Ok(Spanned::new(LoxAst { stmts }, span))
    }

    pub fn parse_declaration<'a>(
        &mut self,
        lexer: &mut StructuredLexer<'a>,
    ) -> PResult<'a, Spanned<Stmt>> {
        log::debug!("Parsing declaration");
        let t = any.peek_next(lexer)?;
        let span = lexer.enter_span();
        let stmt = match t.value {
            TokenValue::Var => Stmt::VarDecl(Box::new(self.parse_var_decl(lexer)?)),
            _ => return Ok(self.parse_stmt(lexer)?),
        };
        Ok(span.end(lexer).spanned(stmt))
    }

    pub fn parse_stmt<'a>(
        &mut self,
        lexer: &mut StructuredLexer<'a>,
    ) -> PResult<'a, Spanned<Stmt>> {
        log::debug!("Parsing statement");
        let span = lexer.enter_span();
        let t = any.peek_next(lexer)?;
        let stmt = match t.value {
            TokenValue::Print => {
                t.consume(lexer);
                let expr = self.expression(lexer, 0)?;
                Stmt::Print(Box::new(expr))
            }
            _ => {
                let expr = self.expression(lexer, 0)?;
                Stmt::Expr(Box::new(expr))
            }
        };
        TokenValue::Semicolon.parse_next(lexer)?;
        Ok(span.end(lexer).spanned(stmt))
    }

    pub fn parse_var_decl<'a>(
        &mut self,
        lexer: &mut StructuredLexer<'a>,
    ) -> PResult<'a, Spanned<VarDecl>> {
        log::debug!("Parsing var declaration");
        let span = lexer.enter_span();
        TokenValue::Var.parse_next(lexer)?;
        let tname = any.parse_next(lexer)?;
        let TokenValue::Ident(name) = tname.value else {
            return Err(ParserError::unexpected_token(tname));
        };
        TokenValue::Equal.parse_next(lexer)?;
        let rhs = (|l: &mut _| self.expression(l, 0)).try_next(lexer);
        TokenValue::Semicolon.parse_next(lexer)?;
        Ok(span.end(lexer).spanned(VarDecl {
            name: tname.lexeme.span.spanned(name),
            rhs,
        }))
    }

    pub fn expression<'a>(
        &mut self,
        lexer: &mut StructuredLexer<'a>,
        starting_precedence: u8,
    ) -> PResult<'a, Spanned<Expr>> {
        log::debug!("Parsing expression at precedence: {starting_precedence}");
        let span = lexer.enter_span();
        let start = any.parse_next(lexer)?;
        let lhs = match start.value {
            TokenValue::LeftParen => {
                let expr = self.expression(lexer, 0)?;
                TokenValue::RightParen.parse_next(lexer)?;
                Expr::group(expr)
            }
            TokenValue::Number(n) => Expr::lit(Lit::Num(Num { value: n })),
            TokenValue::String(s) => Expr::lit(Lit::Str(s)),
            TokenValue::True => Expr::lit(Lit::Bool(true)),
            TokenValue::False => Expr::lit(Lit::Bool(false)),
            TokenValue::Nil => Expr::lit(Lit::Nil),
            _ => {
                if let Ok(op) = PrefixOp::try_from(&start) {
                    let ((), rbp) = op.binding_power();
                    let expr = self.expression(lexer, rbp)?;
                    Expr::unary(op, expr)
                } else {
                    return Err(ParserError::unexpected_token(start));
                }
            }
        };
        let mut lhs = span.end(lexer).spanned(lhs);
        let expr = loop {
            log::debug!("RHS Expression?");
            let token = opt(any).peek_next(lexer)?.transpose();
            let Some(token) = token else {
                break lhs;
            };
            if let Ok(op) = BinaryOp::try_from(&*token) {
                let (lbp, rbp) = op.binding_power();
                if lbp < starting_precedence {
                    break lhs;
                }
                token.consume(lexer);
                let rhs = self.expression(lexer, rbp)?;
                lhs = span.end(lexer).spanned(Expr::binary(lhs, op, rhs));
            } else {
                // Not part of the expression any more so just return the lhs.
                break lhs;
            }
        };
        Ok(expr)
    }
}
