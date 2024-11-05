use lexer::Lexer;
use thiserror::Error;

pub mod ast;
pub mod lexer;

#[derive(Error, Debug)]
pub enum LoxError {}

/// The `Lox` interpreter
pub struct Lox {}

impl Lox {
    pub fn new() -> Self {
        Self {}
    }

    pub fn eval(&mut self, source: &str) -> Result<(), LoxError> {
        let lexer = Lexer::new(source);
        for token in lexer {
            println!("{:?}", token)
        }
        Ok(())
    }
}
