//! Implementation of the Lox abstract syntax tree

use super::lexer::{Token, TokenValue};

use burrow::Burrow;

#[derive(Debug, Clone, Burrow)]
pub struct LoxAst<'a> {
    pub statements: Vec<Stmt<'a>>,
}

#[derive(Debug, Clone, Burrow)]
pub enum Stmt<'a> {
    Print(Box<PrintStmt<'a>>),
    Expr(Box<ExprStmt<'a>>),
    VarDecl(Box<VarDecl<'a>>),
    Block(Box<Block<'a>>),
    If(Box<If<'a>>),
    While(Box<While<'a>>),
    FunDecl(Box<FunDecl<'a>>),
}

#[derive(Debug, Clone, Burrow)]
pub struct FunDecl<'a> {
    pub name: Ident<'a>,
    pub params: Vec<Ident<'a>>,
    pub body: Block<'a>,
}

#[derive(Debug, Clone, Burrow)]
pub struct While<'a> {
    pub cond: Expr<'a>,
    pub body: Stmt<'a>,
}

#[derive(Debug, Clone, Burrow)]
pub struct If<'a> {
    pub cond: Expr<'a>,
    pub then: Stmt<'a>,
    pub alt: Option<Stmt<'a>>,
}

#[derive(Debug, Clone, Burrow)]
pub struct Block<'a> {
    pub stmts: Vec<Stmt<'a>>,
}

#[derive(Debug, Clone, Burrow)]
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

#[derive(Debug, Clone, Burrow)]
pub struct PrintStmt<'a> {
    pub expr: Expr<'a>,
}

#[derive(Debug, Clone, Burrow)]
pub struct ExprStmt<'a> {
    pub expr: Expr<'a>,
}

#[derive(Debug, Clone, Burrow)]
pub enum Expr<'a> {
    Literal(Box<Literal<'a>>),
    Unary(Box<UnaryExpr<'a>>),
    Binary(Box<BinaryExpr<'a>>),
    Grouping(Box<Grouping<'a>>),
    Ident(Box<Ident<'a>>),
    Assignment(Box<Assignment<'a>>),
    Logical(Box<LogicalExpr<'a>>),
    Call(Box<Call<'a>>),
}

#[derive(Debug, Clone, Burrow)]
pub struct Call<'a> {
    pub callee: Expr<'a>,
    pub args: Vec<Expr<'a>>,
}

#[derive(Debug, Clone, Burrow)]
pub struct LogicalExpr<'a> {
    pub lhs: Expr<'a>,
    pub rhs: Expr<'a>,
    pub op: Token<'a>,
}

#[derive(Debug, Clone, Burrow)]
pub struct Assignment<'a> {
    pub lhs: Ident<'a>,
    pub rhs: Expr<'a>,
}

#[derive(Debug, Clone, Burrow)]
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

#[derive(Debug, Clone, Burrow)]
pub struct Literal<'a> {
    pub value: Token<'a>,
}

#[derive(Debug, Clone, Burrow)]
pub struct UnaryExpr<'a> {
    pub op: Token<'a>,
    pub rhs: Expr<'a>,
}

#[derive(Debug, Clone, Burrow)]
pub struct BinaryExpr<'a> {
    pub lhs: Expr<'a>,
    pub rhs: Expr<'a>,
    pub op: Token<'a>,
}

#[derive(Debug, Clone, Burrow)]
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
            Expr::Logical(expr) => visitor.visit_logical_expr(expr),
            Expr::Call(call) => visitor.visit_call(call),
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
            Stmt::While(stmt) => visitor.visit_while(stmt),
            Stmt::FunDecl(d) => visitor.visit_fun_decl(d),
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
    fn visit_logical_expr(&mut self, expr: &LogicalExpr<'a>) -> Self::Output;
    fn visit_while(&mut self, stmt: &While<'a>) -> Self::Output;
    fn visit_call(&mut self, call: &Call<'a>) -> Self::Output;
    fn visit_fun_decl(&mut self, call: &FunDecl<'a>) -> Self::Output;
}
