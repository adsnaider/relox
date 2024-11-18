#![allow(unused_variables)]
use crate::lox::ast::*;

pub struct AstPrinter;

impl<'a> AstPrinter {
    pub fn print(&mut self, expr: &Expr<'a>) {
        self.visit_expr(expr);
        println!("");
    }

    fn print_recursive(&mut self, name: &str, exprs: &[&Expr]) {
        print!("({name}");
        for expr in exprs {
            print!(" ");
            self.visit_expr(expr);
        }
        print!(")");
    }
}
impl<'a> AstVisitor<'a> for AstPrinter {
    type Output = ();

    fn visit_ast(&mut self, ast: &LoxAst) -> Self::Output {
        ast.walk(self, |_, _| {});
    }

    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> Self::Output {
        print!("Stmt ");
        self.visit_expr(&stmt.expr)
    }

    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> Self::Output {
        print!("Print ");
        self.visit_expr(&stmt.expr)
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::Output {
        stmt.walk(self)
    }

    fn visit_expr(&mut self, expr: &Expr) -> Self::Output {
        expr.walk(self)
    }

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

    fn visit_group_expr(&mut self, group_expr: &Grouping) -> Self::Output {
        self.print_recursive("group", &[&group_expr.expr])
    }

    fn visit_var_decl(&mut self, decl: &VarDecl<'a>) -> Self::Output {
        todo!();
    }

    fn visit_ident(&mut self, ident: &Ident<'a>) -> Self::Output {
        todo!()
    }

    fn visit_assign(&mut self, assign: &Assignment<'a>) -> Self::Output {
        todo!()
    }

    fn visit_block(&mut self, block: &Block<'a>) -> Self::Output {
        todo!()
    }

    fn visit_if(&mut self, stmt: &If<'a>) -> Self::Output {
        todo!()
    }

    fn visit_logical_expr(&mut self, expr: &LogicalExpr<'a>) -> Self::Output {
        todo!()
    }

    fn visit_while(&mut self, stmt: &While<'a>) -> Self::Output {
        todo!()
    }

    fn visit_call(&mut self, call: &Call<'a>) -> Self::Output {
        todo!()
    }

    fn visit_fun_decl(&mut self, decl: &FunDecl<'a>) -> Self::Output {
        todo!()
    }

    fn visit_return_stmt(&mut self, ret: &ReturnStmt<'a>) -> Self::Output {
        todo!()
    }
}
