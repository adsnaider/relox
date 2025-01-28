pub mod ast;
pub mod chunk;
pub mod compiler;
pub mod lexer;
pub mod parser;
pub mod value;
pub mod vm;

use std::fmt::Display;

use compiler::Compiler;
use derive_more::derive::From;
use lexer::Lexer;
use parser::{Parser, ParserError};
use vm::{RuntimeError, Vm};

/// The `Lox` interpreter
#[derive(Debug)]
pub struct Lox {
    vm: Vm,
}

#[derive(From, Debug)]
pub enum LoxError<'a> {
    ParseError(#[from] ParserError<'a>),
    RuntimeError(#[from] RuntimeError),
}

impl Display for LoxError<'_> {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Lox {
    pub fn new() -> Self {
        Self { vm: Vm::default() }
    }

    pub fn new_with(vm: Vm) -> Self {
        Self { vm }
    }

    pub fn eval<'a>(&mut self, source: &'a str) -> Result<(), LoxError<'a>> {
        let lexer = Lexer::new(source);
        let ast = Parser::parse(lexer)?;
        let chunk = Compiler::compile(ast.node);
        self.vm.interpret(chunk)?;
        Ok(())
    }
}
