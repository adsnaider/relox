pub mod chunk;
pub mod lexer;
pub mod value;
pub mod vm;

use derive_more::derive::{Display, From};
use lexer::{LexError, Lexer};
use vm::Vm;

/// The `Lox` interpreter
#[derive(Debug)]
pub struct Lox {
    vm: Vm,
}

#[derive(From, Display, Debug)]
pub enum LoxError<'a> {
    #[display("{_0}")]
    LexError(#[from] LexError<'a>),
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
        let mut last_line = 0;
        for token in lexer {
            let token = token?;
            let (line, _) = token.lexeme.locate();
            let cont = if line != last_line {
                format!("{line}")
            } else {
                format!("|")
            };
            println!("{cont:>4} {:?} {}", token.value, token.lexeme);
            last_line = line;
        }
        Ok(())
    }
}
