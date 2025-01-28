mod helpers;
pub mod span;

pub mod visit;

use derive_more::derive::Display;
use span::Spanned;

#[derive(Debug, Clone)]
pub struct LoxAst {
    pub expr: Spanned<Expr>,
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
}

#[derive(Debug, Clone)]
pub struct Group {
    pub expr: Spanned<Expr>,
}

#[derive(Debug, Clone, Display)]
pub enum Lit {
    Num(Num),
}

#[derive(Debug, Clone, Display)]
pub struct Num {
    pub value: f64,
}
