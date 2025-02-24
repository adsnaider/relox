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
use miette::{Diagnostic, GraphicalReportHandler};
use parser::{Parser, ParserError};
use vm::{RuntimeError, Vm};

/// The `Lox` interpreter
#[derive(Debug)]
pub struct Lox {
    vm: Vm,
    compiler: Compiler,
}

#[derive(From, Debug, Error, Diagnostic, Display)]
pub enum LoxErrorKind<'a> {
    #[display("Parse error")]
    ParseError(
        #[related]
        #[error(not(source))]
        Vec<ParserError<'a>>,
    ),
    #[diagnostic(transparent)]
    RuntimeError(RuntimeError),
}

#[derive(Error, Diagnostic, Display)]
#[display("{kind}")]
#[diagnostic(forward(kind))]
pub struct LoxError<'a> {
    #[error(not(source))]
    #[source_code]
    source: &'a str,
    kind: LoxErrorKind<'a>,
}

impl core::fmt::Debug for LoxError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let handler = GraphicalReportHandler::new();
        handler.render_report(f, self)
    }
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
        Self {
            vm: Vm::default(),
            compiler: Compiler::new(),
        }
    }

    pub fn eval<'a>(&mut self, source: &'a str) -> Result<(), LoxError<'a>> {
        let lexer = Lexer::new(source);
        let ast = Parser::parse(lexer).add_ctx(source)?;
        let chunk = self.compiler.compile(ast.node);
        self.vm.interpret(chunk).add_ctx(source)?;
        Ok(())
    }
}
