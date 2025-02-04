pub mod ast;
pub mod chunk;
pub mod compiler;
pub mod lexer;
pub mod parser;
pub mod value;
pub mod vm;

use compiler::Compiler;
use derive_more::derive::{Display, Error, From};
use lexer::Lexer;
use miette::Diagnostic;
use parser::{Parser, ParserError};
use vm::{RuntimeError, Vm};

/// The `Lox` interpreter
#[derive(Debug)]
pub struct Lox {
    vm: Vm,
}

#[derive(From, Debug, Error, Diagnostic, Display)]
pub enum LoxErrorKind<'a> {
    #[diagnostic(transparent)]
    ParseError(#[error(not(source))] ParserError<'a>),
    #[diagnostic(transparent)]
    RuntimeError(RuntimeError),
}

#[derive(Debug, Error, Diagnostic, Display)]
#[display("{kind}")]
#[diagnostic(forward(kind))]
pub struct LoxError<'a> {
    #[error(not(source))]
    #[source_code]
    source: &'a str,
    kind: LoxErrorKind<'a>,
}

trait LoxErrorCtx<'a, T> {
    fn add_ctx(self, source: &'a str) -> Result<T, LoxError<'a>>;
}

impl<'a, T, E: Into<LoxErrorKind<'a>>> LoxErrorCtx<'a, T> for Result<T, E> {
    fn add_ctx(self, source: &'a str) -> Result<T, LoxError<'a>> {
        self.map_err(|e| LoxError {
            source,
            kind: e.into(),
        })
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
        let ast = Parser::parse(lexer).add_ctx(source)?;
        let chunk = Compiler::compile(ast.node);
        // println!("{}", chunk.disassemble());
        self.vm.interpret(chunk).add_ctx(source)?;
        Ok(())
    }
}
