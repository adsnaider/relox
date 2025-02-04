use super::{
    ast::{self, visit::AstVisitor, BinaryOp, LoxAst, PrefixOp},
    chunk::{Chunk, Instr},
    value::Value,
};

#[derive(Debug, Default)]
pub struct Compiler {
    bytecode: Chunk,
}

impl Compiler {
    pub fn compile(ast: LoxAst) -> Chunk {
        let mut compiler = Self {
            bytecode: Chunk::new(),
        };
        compiler.visit_ast(&ast);
        compiler.bytecode
    }
}

impl AstVisitor for Compiler {
    fn visit_ast(&mut self, ast: &LoxAst) {
        ast.walk(self);
        self.bytecode.add_instruction(Instr::Return, 1);
    }

    fn visit_expr(&mut self, expr: &ast::Expr) {
        expr.walk(self)
    }

    fn visit_binary_expr(&mut self, binary_expr: &ast::BinaryExpr) {
        binary_expr.walk(self)
    }

    fn visit_prefix_expr(&mut self, prefix_expr: &ast::PrefixExpr) {
        prefix_expr.walk(self)
    }

    fn visit_literal(&mut self, literal_expr: &ast::Lit) {
        literal_expr.walk(self)
    }

    fn visit_group(&mut self, group: &ast::Group) {
        group.walk(self)
    }

    fn visit_binary_op(&mut self, op: &BinaryOp) {
        match op {
            BinaryOp::Plus => self.bytecode.add_instruction(Instr::Add, 1),
            BinaryOp::Minus => self.bytecode.add_instruction(Instr::Sub, 1),
            BinaryOp::Mult => self.bytecode.add_instruction(Instr::Mul, 1),
            BinaryOp::Div => self.bytecode.add_instruction(Instr::Div, 1),
        }
    }

    fn visit_prefix_op(&mut self, op: &PrefixOp) {
        match op {
            PrefixOp::Plus => {}
            PrefixOp::Neg => self.bytecode.add_instruction(Instr::Negate, 1),
        }
    }

    fn visit_num(&mut self, num: &ast::Num) {
        let idx = self.bytecode.add_constant(Value::num(num.value));
        self.bytecode.add_instruction(Instr::Const(idx), 1);
    }
}
