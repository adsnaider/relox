//! Implementation of the Lox abstract syntax tree

use super::lexer::{Token, TokenValue};

#[derive(Debug)]
pub struct LoxAst<'a> {
    pub statements: Vec<Stmt<'a>>,
}

#[derive(Debug)]
pub enum Stmt<'a> {
    Print(Box<PrintStmt<'a>>),
    Expr(Box<ExprStmt<'a>>),
    VarDecl(Box<VarDecl<'a>>),
    Block(Box<Block<'a>>),
    If(Box<If<'a>>),
}

#[derive(Debug)]
pub struct If<'a> {
    pub cond: Expr<'a>,
    pub then: Stmt<'a>,
    pub alt: Option<Stmt<'a>>,
}

#[derive(Debug)]
pub struct Block<'a> {
    pub stmts: Vec<Stmt<'a>>,
}

#[derive(Debug)]
pub struct VarDecl<'a> {
    pub ident: Ident<'a>,
    pub rhs: Option<Expr<'a>>,
}

impl<'a> VarDecl<'a> {
    pub fn name(&self) -> String {
        self.ident.name()
    }

    pub fn initializer(&self) -> Option<&Expr<'a>> {
        self.rhs.as_ref()
    }
}

#[derive(Debug)]
pub struct PrintStmt<'a> {
    pub expr: Expr<'a>,
}

#[derive(Debug)]
pub struct ExprStmt<'a> {
    pub expr: Expr<'a>,
}

#[derive(Debug)]
pub enum Expr<'a> {
    Literal(Box<Literal<'a>>),
    Unary(Box<UnaryExpr<'a>>),
    Binary(Box<BinaryExpr<'a>>),
    Grouping(Box<Grouping<'a>>),
    Ident(Box<Ident<'a>>),
    Assignment(Box<Assignment<'a>>),
}

#[derive(Debug)]
pub struct Assignment<'a> {
    pub lhs: Ident<'a>,
    pub rhs: Expr<'a>,
}

#[derive(Debug, Clone)]
pub struct Ident<'a> {
    pub ident: Token<'a>,
}

impl<'a> Ident<'a> {
    pub fn name(&self) -> String {
        let TokenValue::Ident(ref i) = self.ident.value else {
            unreachable!("Got an unexpected token in a variable declaration");
        };
        i.clone()
    }
}

#[derive(Debug)]
pub struct Literal<'a> {
    pub value: Token<'a>,
}

#[derive(Debug)]
pub struct UnaryExpr<'a> {
    pub op: Token<'a>,
    pub rhs: Expr<'a>,
}

#[derive(Debug)]
pub struct BinaryExpr<'a> {
    pub lhs: Expr<'a>,
    pub rhs: Expr<'a>,
    pub op: Token<'a>,
}

#[derive(Debug)]
pub struct Grouping<'a> {
    pub expr: Expr<'a>,
}

impl<'a> Expr<'a> {
    pub fn walk<V: AstVisitor<'a>>(&self, visitor: &mut V) -> V::Output {
        match self {
            Expr::Literal(lit) => visitor.visit_literal(lit),
            Expr::Unary(unary) => visitor.visit_unary_expr(unary),
            Expr::Binary(binary) => visitor.visit_binary_expr(binary),
            Expr::Grouping(group) => visitor.visit_group_expr(group),
            Expr::Ident(ident) => visitor.visit_ident(ident),
            Expr::Assignment(assign) => visitor.visit_assign(assign),
        }
    }
}

impl<'a> Stmt<'a> {
    pub fn walk<V: AstVisitor<'a>>(&self, visitor: &mut V) -> V::Output {
        match self {
            Stmt::Print(p) => visitor.visit_print_stmt(p),
            Stmt::Expr(e) => visitor.visit_expr_stmt(e),
            Stmt::VarDecl(d) => visitor.visit_var_decl(d),
            Stmt::Block(b) => visitor.visit_block(b),
            Stmt::If(stmt) => visitor.visit_if(stmt),
        }
    }
}

impl<'a> LoxAst<'a> {
    pub fn walk<F, V>(&self, visitor: &mut V, fold: F) -> Option<V::Output>
    where
        V: AstVisitor<'a>,
        F: FnMut(V::Output, V::Output) -> V::Output,
    {
        self.statements
            .iter()
            .map(|stmt| visitor.visit_stmt(stmt))
            .reduce(fold)
    }
}

pub trait AstVisitor<'a>: Sized {
    type Output;

    fn visit_ast(&mut self, ast: &LoxAst<'a>) -> Self::Output;
    fn visit_stmt(&mut self, stmt: &Stmt<'a>) -> Self::Output {
        stmt.walk(self)
    }
    fn visit_expr_stmt(&mut self, stmt: &ExprStmt<'a>) -> Self::Output;
    fn visit_print_stmt(&mut self, stmt: &PrintStmt<'a>) -> Self::Output;
    fn visit_var_decl(&mut self, decl: &VarDecl<'a>) -> Self::Output;
    fn visit_expr(&mut self, expr: &Expr<'a>) -> Self::Output {
        expr.walk(self)
    }
    fn visit_binary_expr(&mut self, binary_expr: &BinaryExpr<'a>) -> Self::Output;
    fn visit_unary_expr(&mut self, unary_expr: &UnaryExpr<'a>) -> Self::Output;
    fn visit_literal(&mut self, literal_expr: &Literal<'a>) -> Self::Output;
    fn visit_group_expr(&mut self, group_expr: &Grouping<'a>) -> Self::Output;
    fn visit_ident(&mut self, ident: &Ident<'a>) -> Self::Output;
    fn visit_assign(&mut self, assign: &Assignment<'a>) -> Self::Output;
    fn visit_block(&mut self, block: &Block<'a>) -> Self::Output;
    fn visit_if(&mut self, stmt: &If<'a>) -> Self::Output;
}
