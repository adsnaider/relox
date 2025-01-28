mod combine;

use combine::{any, opt, Parse, StructuredLexer};

use crate::lox::ast::BinaryOp;

use super::{
    ast::{span::Spanned, Expr, Lit, LoxAst, Num, PrefixOp},
    lexer::{LexError, Lexer, Token, TokenValue},
};

pub struct Parser<'a> {
    lexer: StructuredLexer<'a>,
}

#[derive(Debug)]
pub enum ParserError<'a> {
    LexerError(Vec<LexError<'a>>),
    UnexpectedEof,
    UnexpectedToken(Token<'a>),
    Fail,
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
            _ => {
                if let Ok(op) = PrefixOp::try_from(&start) {
                    let ((), rbp) = op.binding_power();
                    let expr = self.expression(rbp)?;
                    Expr::unary(op, expr)
                } else {
                    return Err(ParserError::UnexpectedToken(start));
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
