use super::{
    ast::{self, visit::AstVisitor, BinaryOp, Lit, LoxAst, PrefixOp},
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

    fn visit_literal(&mut self, literal_expr: &Lit) {
        match literal_expr {
            Lit::Num(num) => {
                let idx = self.bytecode.add_constant(Value::num(num.value));
                self.bytecode.add_instruction(Instr::Const(idx), 1);
            }
            Lit::Bool(true) => {
                self.bytecode.add_instruction(Instr::True, 1);
            }
            Lit::Bool(false) => {
                self.bytecode.add_instruction(Instr::False, 1);
            }
            Lit::Nil => {
                self.bytecode.add_instruction(Instr::Nil, 1);
            }
            Lit::Str(s) => {
                let idx = self.bytecode.add_constant(Value::str(s.clone()));
                self.bytecode.add_instruction(Instr::Const(idx), 1);
            }
        }
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
            BinaryOp::Equal => self.bytecode.add_instruction(Instr::Eq, 1),
            BinaryOp::NotEqual => {
                self.bytecode.add_instruction(Instr::Eq, 1);
                self.bytecode.add_instruction(Instr::Not, 1);
            }
            BinaryOp::Less => self.bytecode.add_instruction(Instr::Less, 1),
            BinaryOp::Greater => self.bytecode.add_instruction(Instr::Greater, 1),
            BinaryOp::LessEq => {
                self.bytecode.add_instruction(Instr::Greater, 1);
                self.bytecode.add_instruction(Instr::Not, 1);
            }
            BinaryOp::GreaterEq => {
                self.bytecode.add_instruction(Instr::Less, 1);
                self.bytecode.add_instruction(Instr::Not, 1);
            }
        }
    }

    fn visit_prefix_op(&mut self, op: &PrefixOp) {
        match op {
            PrefixOp::Plus => {}
            PrefixOp::Neg => self.bytecode.add_instruction(Instr::Negate, 1),
            PrefixOp::Not => self.bytecode.add_instruction(Instr::Not, 1),
        }
    }
}
