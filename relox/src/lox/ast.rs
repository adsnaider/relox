mod helpers;
pub mod span;

pub mod visit;

use derive_more::derive::Display;
use span::Spanned;
use visit::AstVisitor;

#[derive(Debug, Clone)]
pub struct LoxAst {
    pub stmts: Vec<Spanned<Stmt>>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Box<Spanned<Expr>>),
    Print(Box<Spanned<Expr>>),
    VarDecl(Box<Spanned<VarDecl>>),
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: Spanned<String>,
    pub rhs: Option<Spanned<Expr>>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Lit(Box<Lit>),
    Group(Box<Group>),
    Binary(Box<BinaryExpr>),
    PrefixExpr(Box<PrefixExpr>),
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub lhs: Spanned<Expr>,
    pub rhs: Spanned<Expr>,
    pub op: BinaryOp,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Display)]
pub enum BinaryOp {
    #[display("+")]
    Plus,
    #[display("-")]
    Minus,
    #[display("*")]
    Mult,
    #[display("/")]
    Div,
    #[display("==")]
    Equal,
    #[display("!=")]
    NotEqual,
    #[display("<")]
    Less,
    #[display(">")]
    Greater,
    #[display("<=")]
    LessEq,
    #[display(">=")]
    GreaterEq,
}

#[derive(Debug, Clone)]
pub struct PrefixExpr {
    pub rhs: Spanned<Expr>,
    pub op: PrefixOp,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Display)]
pub enum PrefixOp {
    #[display("+")]
    Plus,
    #[display("-")]
    Neg,
    #[display("!")]
    Not,
}

#[derive(Debug, Clone)]
pub struct Group {
    pub expr: Spanned<Expr>,
}

#[derive(Debug, Clone, Display)]
pub enum Lit {
    Num(Num),
    Bool(bool),
    Nil,
    Str(String),
}

#[derive(Debug, Clone, Display)]
pub struct Num {
    pub value: f64,
}

impl LoxAst {
    pub fn walk<V: AstVisitor>(&self, visitor: &mut V) {
        for stmt in &self.stmts {
            visitor.visit_stmt(&stmt.node)
        }
    }
}

impl Stmt {
    pub fn walk<V: AstVisitor>(&self, visitor: &mut V) {
        match self {
            Stmt::Expr(expr) => visitor.visit_expr_stmt(&&expr.node),
            Stmt::Print(expr) => visitor.visit_print_stmt(&expr.node),
            Stmt::VarDecl(decl) => visitor.visit_var_decl(&decl.node),
        }
    }
}

impl VarDecl {
    pub fn walk<V: AstVisitor>(&self, visitor: &mut V) {
        if let Some(expr) = &self.rhs {
            visitor.visit_expr(&expr.node);
        }
    }
}

impl Expr {
    pub fn walk<V: AstVisitor>(&self, visitor: &mut V) {
        match self {
            Expr::Lit(lit) => visitor.visit_literal(lit),
            Expr::Group(group) => visitor.visit_group(group),
            Expr::Binary(binary_expr) => visitor.visit_binary_expr(binary_expr),
            Expr::PrefixExpr(prefix_expr) => visitor.visit_prefix_expr(prefix_expr),
        }
    }
}

impl BinaryExpr {
    pub fn walk<V: AstVisitor>(&self, visitor: &mut V) {
        visitor.visit_expr(&self.lhs.node);
        visitor.visit_expr(&self.rhs.node);
        visitor.visit_binary_op(&self.op);
    }
}

impl PrefixExpr {
    pub fn walk<V: AstVisitor>(&self, visitor: &mut V) {
        visitor.visit_expr(&self.rhs.node);
        visitor.visit_prefix_op(&self.op);
    }
}

impl Lit {
    pub fn walk<V: AstVisitor>(&self, _visitor: &mut V) {}
}

impl Group {
    pub fn walk<V: AstVisitor>(&self, visitor: &mut V) {
        visitor.visit_expr(&self.expr.node);
    }
}
