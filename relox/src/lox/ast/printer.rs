use super::{BinaryExpr, Expr, ExprVisitor, Literal, UnaryExpr};

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
