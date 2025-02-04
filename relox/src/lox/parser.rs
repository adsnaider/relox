mod combine;

use combine::{any, opt, Parse, StructuredLexer};
use miette::Diagnostic;
use ownit::Ownit;
use thiserror::Error;

use crate::lox::ast::BinaryOp;

use super::{
    ast::{span::Spanned, Expr, Lit, LoxAst, Num, PrefixOp},
    lexer::{LexError, Lexer, Token, TokenValue},
};

pub struct Parser<'a> {
    lexer: StructuredLexer<'a>,
}

#[derive(Debug, Error, Diagnostic, Ownit)]
pub enum ParserError<'a> {
    #[error("Failure during lexing")]
    LexerError(#[related] Vec<LexError<'a>>),
    #[error("Unexpected EOF")]
    UnexpectedEof,
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
}

#[derive(Debug, Error, Diagnostic, Ownit)]
#[error("Unexpected token: {}", .token.lexeme)]
#[diagnostic(code(parser::unexpected_token))]
pub struct UnexpectedToken<'a> {
    #[label("Here")]
    #[source_code]
    token: Token<'a>,
}

pub type PResult<'a, T> = Result<T, ParserError<'a>>;

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer: StructuredLexer::new(lexer),
        }
    }

    pub fn parse(lexer: Lexer<'a>) -> PResult<'a, Spanned<LoxAst>> {
        let mut this = Self::new(lexer);
        this.ast()
    }

    pub fn ast(&mut self) -> PResult<'a, Spanned<LoxAst>> {
        log::debug!("Parsing the AST");
        let expr = self.expression(0)?;
        let span = expr.span;
        Ok(Spanned::new(LoxAst { expr }, span))
    }

    pub fn expression(&mut self, starting_precedence: u8) -> PResult<'a, Spanned<Expr>> {
        log::debug!("Parsing expression at precedence: {starting_precedence}");
        let span = self.lexer.enter_span();
        let start = any.parse_next(&mut self.lexer)?;
        let lhs = match start.value {
            TokenValue::LeftParen => {
                let expr = self.expression(0)?;
                TokenValue::RightParen.parse_next(&mut self.lexer)?;
                Expr::group(expr)
            }
            TokenValue::Number(n) => Expr::lit(Lit::Num(Num { value: n })),
            TokenValue::True => Expr::lit(Lit::Bool(true)),
            TokenValue::False => Expr::lit(Lit::Bool(false)),
            TokenValue::Nil => Expr::lit(Lit::Nil),
            _ => {
                if let Ok(op) = PrefixOp::try_from(&start) {
                    let ((), rbp) = op.binding_power();
                    let expr = self.expression(rbp)?;
                    Expr::unary(op, expr)
                } else {
                    return Err(ParserError::unexpected_token(start));
                }
            }
        };
        let mut lhs = span.end(&mut self.lexer).spanned(lhs);
        let expr = loop {
            let token = opt(any).peek_next(&mut self.lexer)?.transpose();
            let Some(token) = token else {
                break lhs;
            };
            if let Ok(op) = BinaryOp::try_from(&*token) {
                let (lbp, rbp) = op.binding_power();
                if lbp < starting_precedence {
                    break lhs;
                }
                token.consume(&mut self.lexer);
                let rhs = self.expression(rbp)?;
                lhs = span
                    .end(&mut self.lexer)
                    .spanned(Expr::binary(lhs, op, rhs));
            } else {
                // Not part of the expression any more so just return the lhs.
                break lhs;
            }
        };
        Ok(expr)
    }
}
