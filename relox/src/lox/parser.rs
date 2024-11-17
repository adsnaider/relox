use crate::lox::lexer::Lexeme;

use super::{
    ast::{
        Assignment, BinaryExpr, Block, Expr, ExprStmt, Ident, If, Literal, LogicalExpr, LoxAst,
        PrintStmt, Stmt, UnaryExpr, VarDecl, While,
    },
    lexer::{Token, TokenValue, TokenVariants},
};

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    cursor: usize,
}

#[derive(Debug)]
pub enum ParseError<'a> {
    UnexpectedToken(Token<'a>),
    InvalidAssignment(Token<'a>),
}

impl ParseError<'_> {
    pub fn to_owned(self) -> ParseError<'static> {
        match self {
            ParseError::UnexpectedToken(t) => ParseError::UnexpectedToken(t.to_owned()),
            ParseError::InvalidAssignment(t) => ParseError::InvalidAssignment(t.to_owned()),
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

    pub fn parse(&mut self) -> Result<LoxAst<'a>, ParseError<'a>> {
        let mut statements = Vec::new();
        while let Err(_) = self.eat_matches(&[TokenVariants::Eof]) {
            statements.push(self.parse_decl()?);
        }
        Ok(LoxAst { statements })
    }

    pub fn parse_decl(&mut self) -> Result<Stmt<'a>, ParseError<'a>> {
        self.try_parse_var_decl()
            .unwrap_or_else(|| self.parse_stmt())
    }

    pub fn parse_var_decl(&mut self) -> Result<Stmt<'a>, ParseError<'a>> {
        self.try_parse_var_decl()
            .ok_or_else(|| ParseError::UnexpectedToken(self.peekn(0).clone()))?
    }

    pub fn try_parse_var_decl(&mut self) -> Option<Result<Stmt<'a>, ParseError<'a>>> {
        self.eat_matches(&[TokenVariants::Var]).ok()?;
        let mut inner = || {
            let ident = self.eat_matches(&[TokenVariants::Ident])?;
            let rhs = self
                .eat_matches(&[TokenVariants::Equal])
                .ok()
                .map(|_| {
                    let expr = self.parse_expr()?;
                    Ok(expr)
                })
                .transpose()?;
            self.eat_matches(&[TokenVariants::Semicolon])?;
            Ok(Stmt::VarDecl(Box::new(VarDecl {
                ident: Ident { ident },
                rhs,
            })))
        };
        Some(inner())
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt<'a>, ParseError<'a>> {
        self.try_parse_print_statement()
            .or_else(|| self.try_parse_if_stmt())
            .or_else(|| self.try_parse_while_stmt())
            .or_else(|| self.try_parse_for_stmt())
            .or_else(|| self.try_parse_block())
            .unwrap_or_else(|| self.parse_expr_statement())
    }

    pub fn try_parse_for_stmt(&mut self) -> Option<Result<Stmt<'a>, ParseError<'a>>> {
        self.eat_matches(&[TokenVariants::For]).ok()?;
        let mut inner = || {
            self.eat_matches(&[TokenVariants::LeftParen])?;
            let init = if let Some(stmt) = self.try_parse_var_decl().transpose()? {
                Some(stmt)
            } else if let Ok(_) = self.eat_matches(&[TokenVariants::Semicolon]) {
                None
            } else {
                Some(self.parse_expr_statement()?)
            };

            let cond = if self.eat_matches(&[TokenVariants::Semicolon]).is_ok() {
                None
            } else {
                let cond = self.parse_expr()?;
                self.eat_matches(&[TokenVariants::Semicolon])?;
                Some(cond)
            };

            let increment = if self.eat_matches(&[TokenVariants::RightParen]).is_ok() {
                None
            } else {
                let inc = self.parse_expr()?;
                self.eat_matches(&[TokenVariants::RightParen])?;
                Some(inc)
            };
            let body = self.parse_stmt()?;

            let body = {
                let mut body = vec![body];
                if let Some(increment) = increment {
                    body.push(Stmt::Expr(Box::new(ExprStmt { expr: increment })));
                }
                Stmt::Block(Box::new(Block { stmts: body }))
            };
            let block = {
                let mut stmts = Vec::new();
                if let Some(init) = init {
                    stmts.push(init);
                }
                let cond = cond.unwrap_or_else(|| {
                    Expr::Literal(Box::new(Literal {
                        value: Token {
                            value: TokenValue::True,
                            lexeme: Lexeme::from_str("", 0),
                        },
                    }))
                });
                stmts.push(Stmt::While(Box::new(While { cond, body })));
                Stmt::Block(Box::new(Block { stmts }))
            };
            Ok(block)
        };
        Some(inner())
    }
    pub fn try_parse_while_stmt(&mut self) -> Option<Result<Stmt<'a>, ParseError<'a>>> {
        self.eat_matches(&[TokenVariants::While]).ok()?;
        let mut inner = || {
            self.eat_matches(&[TokenVariants::LeftParen])?;
            let cond = self.parse_expr()?;
            self.eat_matches(&[TokenVariants::RightParen])?;
            let body = self.parse_stmt()?;

            Ok(Stmt::While(Box::new(While { cond, body })))
        };
        Some(inner())
    }

    pub fn try_parse_if_stmt(&mut self) -> Option<Result<Stmt<'a>, ParseError<'a>>> {
        self.eat_matches(&[TokenVariants::If]).ok()?;
        let mut inner = || {
            self.eat_matches(&[TokenVariants::LeftParen])?;
            let cond = self.parse_expr()?;
            self.eat_matches(&[TokenVariants::RightParen])?;
            let then = self.parse_stmt()?;

            let alt = if self.eat_matches(&[TokenVariants::Else]).is_ok() {
                Some(self.parse_stmt()?)
            } else {
                None
            };
            Ok(Stmt::If(Box::new(If { cond, then, alt })))
        };
        Some(inner())
    }

    pub fn parse_block(&mut self) -> Result<Stmt<'a>, ParseError<'a>> {
        self.try_parse_block()
            .ok_or_else(|| ParseError::UnexpectedToken(self.peekn(0).clone()))?
    }

    pub fn try_parse_block(&mut self) -> Option<Result<Stmt<'a>, ParseError<'a>>> {
        self.eat_matches(&[TokenVariants::LeftBrace]).ok()?;
        let mut inner = || {
            let mut stmts = Vec::new();
            while self.eat_matches(&[TokenVariants::RightBrace]).is_err() {
                stmts.push(self.parse_decl()?);
            }
            Ok(Stmt::Block(Box::new(Block { stmts })))
        };
        Some(inner())
    }

    pub fn try_parse_print_statement(&mut self) -> Option<Result<Stmt<'a>, ParseError<'a>>> {
        self.eat_matches(&[TokenVariants::Print]).ok()?;
        let mut inner = || {
            let expr = self.parse_expr()?;
            self.eat_matches(&[TokenVariants::Semicolon])?;
            Ok(Stmt::Print(Box::new(PrintStmt { expr })))
        };
        Some(inner())
    }

    pub fn parse_print_statement(&mut self) -> Result<Stmt<'a>, ParseError<'a>> {
        self.try_parse_print_statement()
            .ok_or_else(|| ParseError::UnexpectedToken(self.peekn(0).clone()))?
    }

    pub fn parse_expr_statement(&mut self) -> Result<Stmt<'a>, ParseError<'a>> {
        let expr = self.parse_expr()?;
        self.eat_matches(&[TokenVariants::Semicolon])?;
        Ok(Stmt::Expr(Box::new(ExprStmt { expr })))
    }

    pub fn parse_expr(&mut self) -> Result<Expr<'a>, ParseError<'a>> {
        self.parse_assignment()
    }

    pub fn parse_assignment(&mut self) -> Result<Expr<'a>, ParseError<'a>> {
        let mut expr = self.parse_or()?;
        if let Ok(t) = self.eat_matches(&[TokenVariants::Equal]) {
            let rhs = self.parse_or()?;
            let Expr::Ident(lhs) = expr else {
                return Err(ParseError::InvalidAssignment(t));
            };
            expr = Expr::Assignment(Box::new(Assignment { lhs: *lhs, rhs }));
        }
        Ok(expr)
    }

    pub fn parse_or(&mut self) -> Result<Expr<'a>, ParseError<'a>> {
        let mut lhs = self.parse_and()?;
        while let Ok(or) = self.eat_matches(&[TokenVariants::Or]) {
            let rhs = self.parse_and()?;
            lhs = Expr::Logical(Box::new(LogicalExpr { lhs, rhs, op: or }));
        }
        Ok(lhs)
    }

    pub fn parse_and(&mut self) -> Result<Expr<'a>, ParseError<'a>> {
        let mut lhs = self.parse_equality()?;
        while let Ok(or) = self.eat_matches(&[TokenVariants::And]) {
            let rhs = self.parse_equality()?;
            lhs = Expr::Logical(Box::new(LogicalExpr { lhs, rhs, op: or }));
        }
        Ok(lhs)
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
        } else if let Ok(t) = self.eat_matches(&[TokenVariants::Ident]) {
            Ok(Expr::Ident(Box::new(Ident { ident: t })))
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
#[allow(unused)]
impl<'a> Parser<'a> {
    fn peekn(&self, idx: usize) -> &Token<'a> {
        self.tokens.get(self.cursor + idx).unwrap_or(&EOF_TOKEN)
    }

    fn peekn_if<F>(&self, idx: usize, pred: F) -> bool
    where
        F: FnOnce(&Token<'a>) -> bool,
    {
        pred(self.peekn(idx))
    }

    fn peekn_matches(&self, idx: usize, variants: &[TokenVariants]) -> bool {
        self.peekn_if(idx, |t| {
            let variant = TokenVariants::from(&t.value);
            variants.contains(&variant)
        })
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
