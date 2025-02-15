use derive_more::derive::{Deref, Display};
use hashbrown::HashMap;

use super::{
    ast::{self, visit::AstVisitor, BinaryOp, Lit, LoxAst, PrefixOp},
    chunk::{Chunk, ConstValue, Instr},
};

#[derive(Deref, Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Display, Hash)]
pub struct GlobalId(u16);

impl From<u16> for GlobalId {
    fn from(value: u16) -> Self {
        Self(value)
    }
}

#[derive(Debug, Default)]
pub struct Compiler {
    bytecode: Chunk,
    globals: HashMap<String, GlobalId>,
}

impl Compiler {
    pub fn compile(&mut self, ast: LoxAst) -> Chunk {
        let globals = {
            let mut resolver = GlobalResolver::default();
            resolver.visit_ast(&ast);
            resolver.globals
        };
        self.globals.extend(globals);
        self.visit_ast(&ast);
        core::mem::take(&mut self.bytecode)
    }

    pub fn new() -> Self {
        Self {
            bytecode: Chunk::new(),
            globals: HashMap::new(),
        }
    }
}

#[derive(Default, Debug)]
pub struct GlobalResolver {
    globals: HashMap<String, GlobalId>,
    next_id: GlobalId,
}

impl AstVisitor for GlobalResolver {
    fn visit_var_decl(&mut self, var_decl: &ast::VarDecl) {
        let id = self.next_id;
        self.globals
            .try_insert(var_decl.name.node.clone(), id)
            .unwrap();
        self.next_id = GlobalId(self.next_id.0 + 1);
    }
}

impl AstVisitor for Compiler {
    fn visit_print_stmt(&mut self, expr: &ast::Expr) {
        expr.walk(self);
        self.bytecode.add_instruction(Instr::Print, 1);
    }

    fn visit_expr_stmt(&mut self, expr: &ast::Expr) {
        expr.walk(self);
        self.bytecode.add_instruction(Instr::Pop, 1);
    }

    fn visit_var_decl(&mut self, var_decl: &ast::VarDecl) {
        let global = self.globals[&var_decl.name.node];
        match &var_decl.rhs {
            Some(expr) => self.visit_expr(&expr.node),
            None => self.bytecode.add_instruction(Instr::Nil, 1),
        }
        self.bytecode
            .add_instruction(Instr::DefineGlobal(global), 1);
    }

    fn visit_literal(&mut self, literal_expr: &Lit) {
        match literal_expr {
            Lit::Num(num) => {
                let idx = self.bytecode.add_constant(ConstValue::Num(num.value));
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
                let idx = self.bytecode.add_constant(ConstValue::Str(s.clone()));
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

    fn visit_ident(&mut self, ident: &ast::Ident) {
        let global = self.globals.get(&ident.name).unwrap();
        self.bytecode.add_instruction(Instr::GetGlobal(*global), 1);
    }
}
