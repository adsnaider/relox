use crate::lox::lexer::Lexeme;

use super::{
    ast::{BinaryExpr, Expr, Literal, UnaryExpr},
    lexer::{Token, TokenValue, TokenVariants},
};

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    cursor: usize,
}

#[derive(Debug)]
pub enum ParseError<'a> {
    UnexpectedToken(Token<'a>),
}

impl ParseError<'_> {
    pub fn to_owned(self) -> ParseError<'static> {
        match self {
            ParseError::UnexpectedToken(t) => ParseError::UnexpectedToken(t.to_owned()),
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(tokens: impl IntoIterator<Item = Token<'a>>) -> Self {
        Self {
            tokens: tokens.into_iter().collect(),
            cursor: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Expr<'a>, ParseError<'a>> {
        let expr = self.parse_expr()?;
        self.eat_matches(&[TokenVariants::Eof])?;
        Ok(expr)
    }

    pub fn parse_expr(&mut self) -> Result<Expr<'a>, ParseError<'a>> {
        self.parse_equality()
    }

    pub fn parse_equality(&mut self) -> Result<Expr<'a>, ParseError<'a>> {
        let mut lhs = self.parse_comparison()?;
        while let Ok(op) =
            self.eat_if(|t| matches!(t.value, TokenValue::EqualEqual | TokenValue::BangEqual))
        {
            let rhs = self.parse_comparison()?;
            lhs = Expr::Binary(Box::new(BinaryExpr { lhs, rhs, op }));
        }
        Ok(lhs)
    }

    pub fn parse_comparison(&mut self) -> Result<Expr<'a>, ParseError<'a>> {
        let mut lhs = self.parse_term()?;
        while let Ok(op) = self.eat_if(|t| {
            matches!(
                t.value,
                TokenValue::Greater
                    | TokenValue::GreaterEqual
                    | TokenValue::Less
                    | TokenValue::LessEqual
            )
        }) {
            let rhs = self.parse_term()?;
            lhs = Expr::Binary(Box::new(BinaryExpr { lhs, rhs, op }));
        }
        Ok(lhs)
    }

    pub fn parse_term(&mut self) -> Result<Expr<'a>, ParseError<'a>> {
        let mut lhs = self.parse_factor()?;
        while let Ok(op) = self.eat_if(|t| matches!(t.value, TokenValue::Minus | TokenValue::Plus))
        {
            let rhs = self.parse_term()?;
            lhs = Expr::Binary(Box::new(BinaryExpr { lhs, rhs, op }));
        }
        Ok(lhs)
    }

    pub fn parse_factor(&mut self) -> Result<Expr<'a>, ParseError<'a>> {
        let mut lhs = self.parse_unary()?;
        while let Ok(op) = self.eat_if(|t| matches!(t.value, TokenValue::Slash | TokenValue::Star))
        {
            let rhs = self.parse_unary()?;
            lhs = Expr::Binary(Box::new(BinaryExpr { lhs, rhs, op }));
        }
        Ok(lhs)
    }

    pub fn parse_unary(&mut self) -> Result<Expr<'a>, ParseError<'a>> {
        if let Ok(op) = self.eat_if(|t| matches!(t.value, TokenValue::Bang | TokenValue::Minus)) {
            let rhs = self.parse_unary()?;
            Ok(Expr::Unary(Box::new(UnaryExpr { op, rhs })))
        } else {
            self.parse_primary()
        }
    }

    pub fn parse_primary(&mut self) -> Result<Expr<'a>, ParseError<'a>> {
        if let Ok(lit) = self.eat_matches(&[
            TokenVariants::Number,
            TokenVariants::String,
            TokenVariants::True,
            TokenVariants::False,
            TokenVariants::Nil,
        ]) {
            Ok(Expr::Literal(Box::new(Literal { value: lit })))
        } else if let Ok(_paren) = self.eat_matches(&[TokenVariants::LeftParen]) {
            let group = self.parse_expr()?;
            self.eat_matches(&[TokenVariants::RightParen])?;
            Ok(group)
        } else {
            Err(ParseError::UnexpectedToken(self.consume()))
        }
    }
}

static EOF_TOKEN: Token<'static> = Token {
    value: TokenValue::Eof,
    lexeme: Lexeme::from_str("", 0),
};

/// Parser combinators
impl<'a> Parser<'a> {
    fn peekn(&self, idx: usize) -> &Token<'a> {
        self.tokens.get(self.cursor + idx).unwrap_or(&EOF_TOKEN)
    }

    fn eat_if<F>(&mut self, pred: F) -> Result<Token<'a>, ParseError<'a>>
    where
        F: FnOnce(&Token<'a>) -> bool,
    {
        if pred(self.peekn(0)) {
            Ok(self.consume())
        } else {
            Err(ParseError::UnexpectedToken(self.peekn(0).clone()))
        }
    }

    fn consume(&mut self) -> Token<'a> {
        let t = self
            .tokens
            .get(self.cursor)
            .cloned()
            .unwrap_or_else(|| EOF_TOKEN.clone());
        self.cursor += 1;
        t
    }

    fn eat_matches(&mut self, variants: &[TokenVariants]) -> Result<Token<'a>, ParseError<'a>> {
        self.eat_if(|t| {
            let variant = TokenVariants::from(&t.value);
            variants.contains(&variant)
        })
    }
}
