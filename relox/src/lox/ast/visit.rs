use super::*;

pub trait AstVisitor: Sized {
    type Output;

    fn visit_ast(&mut self, ast: &LoxAst) -> Self::Output;
    fn visit_expr(&mut self, expr: &Expr) -> Self::Output;
    fn visit_binary_expr(&mut self, binary_expr: &BinaryExpr) -> Self::Output;
    fn visit_prefix_expr(&mut self, prefix_expr: &PrefixExpr) -> Self::Output;
    fn visit_literal(&mut self, literal_expr: &Lit) -> Self::Output;
    fn visit_group(&mut self, group: &Group) -> Self::Output;
}
