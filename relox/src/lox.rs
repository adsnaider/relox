use thiserror::Error;

mod lexer;

#[derive(Error, Debug)]
pub enum LoxError {}

/// The `Lox` interpreter
pub struct Lox {}

impl Lox {
    pub fn new() -> Self {
        Self {}
    }

    pub fn eval(&self, source: &str) -> Result<(), LoxError> {
        todo!();
    }
}
