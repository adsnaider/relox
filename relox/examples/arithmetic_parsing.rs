use relox::lox::{
    ast::{visit::AstVisitor, BinaryExpr, Expr, Group, Lit, LoxAst, PrefixExpr},
    lexer::Lexer,
    parser::Parser,
};

fn main() {
    env_logger::init();
    assert_eq!(reverse_polished("3 + 4 * 2"), "(+ 3 (* 4 2))");
    assert_eq!(reverse_polished("(3 + 4) * 2"), "(* (+ 3 4) 2)");
    assert_eq!(reverse_polished("((3) + (4) * 2)"), "(+ 3 (* 4 2))");
    assert_eq!(reverse_polished("(-3 + 4) * -2)"), "(* (+ (- 3) 4) (- 2))");
    assert_eq!(
        reverse_polished("(-3 + ++-4) * -2)"),
        "(* (+ (- 3) (+ (+ (- 4)))) (- 2))"
    );
    assert_eq!(reverse_polished("1 + 2 + 3 + 4"), "(+ (+ (+ 1 2) 3) 4)");
}

fn reverse_polished(expr: &str) -> String {
    let lexer = Lexer::new(expr);
    let ast = Parser::parse(lexer).unwrap();
    ReversePolisher.visit_ast(&ast.node)
}

struct ReversePolisher;

impl AstVisitor for ReversePolisher {
    type Output = String;

    fn visit_ast(&mut self, ast: &LoxAst) -> Self::Output {
        self.visit_expr(&ast.expr.node)
    }

    fn visit_expr(&mut self, expr: &Expr) -> Self::Output {
        match expr {
            Expr::Lit(lit) => self.visit_literal(&lit),
            Expr::Group(group) => self.visit_group(&group),
            Expr::Binary(binary_expr) => self.visit_binary_expr(&binary_expr),
            Expr::PrefixExpr(prefix_expr) => self.visit_prefix_expr(&prefix_expr),
        }
    }

    fn visit_binary_expr(&mut self, binary_expr: &BinaryExpr) -> Self::Output {
        format!(
            "({} {} {})",
            binary_expr.op,
            self.visit_expr(&binary_expr.lhs.node),
            self.visit_expr(&binary_expr.rhs.node)
        )
    }

    fn visit_prefix_expr(&mut self, prefix_expr: &PrefixExpr) -> Self::Output {
        format!(
            "({} {})",
            prefix_expr.op,
            self.visit_expr(&prefix_expr.rhs.node),
        )
    }

    fn visit_literal(&mut self, literal_expr: &Lit) -> Self::Output {
        format!("{}", literal_expr)
    }

    fn visit_group(&mut self, group_expr: &Group) -> Self::Output {
        format!("{}", self.visit_expr(&group_expr.expr.node))
    }
}
