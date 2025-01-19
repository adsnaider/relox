pub mod chunk;
pub mod value;

use std::convert::Infallible;

/// The `Lox` interpreter
#[derive(Debug)]
pub struct Lox {}

impl Lox {
    pub fn new() -> Self {
        Self {}
    }

    pub fn eval(&mut self, _source: &str) -> Result<(), Infallible> {
        todo!();
    }
}
