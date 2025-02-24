use super::*;

pub trait AstOutputVisitor: Sized {
    type Output;

    fn visit_ast(&mut self, ast: &LoxAst) -> Self::Output;
    fn visit_expr(&mut self, expr: &Expr) -> Self::Output;
    fn visit_binary_expr(&mut self, binary_expr: &BinaryExpr) -> Self::Output;
    fn visit_prefix_expr(&mut self, prefix_expr: &PrefixExpr) -> Self::Output;
    fn visit_literal(&mut self, literal_expr: &Lit) -> Self::Output;
    fn visit_group(&mut self, group: &Group) -> Self::Output;
}

pub trait AstVisitor: Sized {
    fn visit_ast(&mut self, ast: &LoxAst) {
        ast.walk(self)
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        stmt.walk(self)
    }

    fn visit_expr_stmt(&mut self, expr: &Expr) {
        expr.walk(self)
    }

    fn visit_print_stmt(&mut self, expr: &Expr) {
        expr.walk(self)
    }

    fn visit_var_decl(&mut self, var_decl: &VarDecl) {
        var_decl.walk(self)
    }

    fn visit_assignment(&mut self, assignment: &Assignment) {
        assignment.walk(self)
    }

    fn visit_expr(&mut self, expr: &Expr) {
        expr.walk(self)
    }

    fn visit_binary_expr(&mut self, binary_expr: &BinaryExpr) {
        binary_expr.walk(self)
    }

    fn visit_prefix_expr(&mut self, prefix_expr: &PrefixExpr) {
        prefix_expr.walk(self)
    }

    fn visit_literal(&mut self, literal_expr: &Lit) {
        literal_expr.walk(self)
    }

    fn visit_group(&mut self, group: &Group) {
        group.walk(self)
    }

    fn visit_binary_op(&mut self, _op: &BinaryOp) {}

    fn visit_prefix_op(&mut self, _op: &PrefixOp) {}

    fn visit_ident(&mut self, _ident: &Ident) {}
}
