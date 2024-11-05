//! Implementation of the Lox abstract syntax tree

use super::lexer::Token;

#[derive(Debug)]
pub enum Expr<'a> {
    Literal(Box<Literal<'a>>),
    Unary(Box<UnaryExpr<'a>>),
    Binary(Box<BinaryExpr<'a>>),
    Grouping(Box<Expr<'a>>),
}

#[derive(Debug)]
pub struct Literal<'a> {
    pub value: Token<'a>,
}

#[derive(Debug)]
pub struct UnaryExpr<'a> {
    pub op: Token<'a>,
    pub rhs: Box<Expr<'a>>,
}

#[derive(Debug)]
pub struct BinaryExpr<'a> {
    pub lhs: Box<Expr<'a>>,
    pub rhs: Box<Expr<'a>>,
    pub op: Token<'a>,
}

pub struct Grouping<'a> {
    pub expr: Box<Expr<'a>>,
}

pub struct ExprPrinter;

impl ExprPrinter {
    pub fn print(&mut self, expr: &Expr) {
        expr.visit(self);
        println!("");
    }

    fn print_recursive(&mut self, name: &str, exprs: &[&Expr]) {
        print!("({name}");
        for expr in exprs {
            print!(" ");
            expr.visit(self);
        }
        print!(")");
    }
}
impl ExprVisitor for ExprPrinter {
    type Output = ();

    fn visit_binary_expr(&mut self, binary_expr: &BinaryExpr) -> Self::Output {
        self.print_recursive(
            binary_expr.op.lexeme.as_str(),
            &[&binary_expr.lhs, &binary_expr.rhs],
        )
    }

    fn visit_unary_expr(&mut self, unary_expr: &UnaryExpr) -> Self::Output {
        self.print_recursive(unary_expr.op.lexeme.as_str(), &[&unary_expr.rhs])
    }

    fn visit_literal(&mut self, literal_expr: &Literal) -> Self::Output {
        print!("{}", literal_expr.value.lexeme.as_str());
    }

    fn visit_group_expr(&mut self, group_expr: &Expr) -> Self::Output {
        self.print_recursive("group", &[group_expr])
    }
}

impl Expr<'_> {
    pub fn visit<V: ExprVisitor>(&self, visitor: &mut V) -> V::Output {
        match self {
            Expr::Literal(lit) => visitor.visit_literal(lit),
            Expr::Unary(unary) => visitor.visit_unary_expr(unary),
            Expr::Binary(binary) => visitor.visit_binary_expr(binary),
            Expr::Grouping(group) => visitor.visit_group_expr(group),
        }
    }
}

pub trait ExprVisitor {
    type Output;

    fn visit_binary_expr(&mut self, binary_expr: &BinaryExpr) -> Self::Output;
    fn visit_unary_expr(&mut self, unary_expr: &UnaryExpr) -> Self::Output;
    fn visit_literal(&mut self, literal_expr: &Literal) -> Self::Output;
    fn visit_group_expr(&mut self, group_expr: &Expr) -> Self::Output;
}
