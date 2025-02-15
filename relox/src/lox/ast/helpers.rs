use crate::lox::{
    lexer::{Token, TokenValue},
    parser::ParserError,
};

use super::*;

impl BinaryOp {
    pub fn binding_power(&self) -> (u8, u8) {
        match *self {
            BinaryOp::Plus | BinaryOp::Minus => (5, 6),
            BinaryOp::Mult | BinaryOp::Div => (7, 8),
            BinaryOp::Equal | BinaryOp::NotEqual => (1, 2),
            BinaryOp::Greater | BinaryOp::GreaterEq | BinaryOp::Less | BinaryOp::LessEq => (3, 4),
        }
    }
}

impl PrefixOp {
    pub fn binding_power(&self) -> ((), u8) {
        match *self {
            Self::Plus | Self::Neg | Self::Not => ((), 9),
        }
    }
}

impl<'a> TryFrom<&Token<'a>> for BinaryOp {
    type Error = ParserError<'a>;

    fn try_from(value: &Token<'a>) -> Result<Self, Self::Error> {
        match value.value {
            TokenValue::Plus => Ok(Self::Plus),
            TokenValue::Minus => Ok(Self::Minus),
            TokenValue::Star => Ok(Self::Mult),
            TokenValue::Slash => Ok(Self::Div),
            TokenValue::Less => Ok(Self::Less),
            TokenValue::LessEqual => Ok(Self::LessEq),
            TokenValue::Greater => Ok(Self::Greater),
            TokenValue::GreaterEqual => Ok(Self::GreaterEq),
            TokenValue::EqualEqual => Ok(Self::Equal),
            TokenValue::BangEqual => Ok(Self::NotEqual),
            _ => Err(ParserError::unexpected_token(value.clone())),
        }
    }
}

impl<'a> TryFrom<&Token<'a>> for PrefixOp {
    type Error = ParserError<'a>;

    fn try_from(value: &Token<'a>) -> Result<Self, Self::Error> {
        match value.value {
            TokenValue::Plus => Ok(Self::Plus),
            TokenValue::Minus => Ok(Self::Neg),
            TokenValue::Bang => Ok(Self::Not),
            _ => Err(ParserError::unexpected_token(value.clone())),
        }
    }
}

impl Expr {
    pub fn group(group: Spanned<Expr>) -> Self {
        Self::Group(Box::new(Group { expr: group }))
    }

    pub fn lit(lit: Lit) -> Self {
        Self::Lit(Box::new(lit))
    }

    pub fn binary(lhs: Spanned<Expr>, op: BinaryOp, rhs: Spanned<Expr>) -> Self {
        Self::Binary(Box::new(BinaryExpr { lhs, rhs, op }))
    }

    pub fn unary(op: PrefixOp, rhs: Spanned<Expr>) -> Self {
        Self::PrefixExpr(Box::new(PrefixExpr { op, rhs }))
    }

    pub fn ident(ident: String) -> Self {
        Self::Ident(Box::new(Ident { name: ident }))
    }
}
