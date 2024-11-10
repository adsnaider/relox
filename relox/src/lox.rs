use std::fmt::Display;

use derive_more::{Error, From};
use interpreter::evaluate;
use lexer::{LexError, Lexer};
use parser::{ParseError, Parser};

pub mod ast;
pub mod interpreter;
pub mod lexer;
pub mod parser;

#[derive(Error, Debug)]
pub struct LoxError<'a> {
    content: &'a str,
    kind: LoxErrorKind<'a>,
}

impl LoxError<'_> {
    pub fn report(&self) {
        print!("{self}")
    }
}

#[derive(From, Debug)]
pub enum LoxErrorKind<'a> {
    LexerError(Vec<LexError<'a>>),
    ParseError(ParseError<'a>),
}

impl Display for LoxError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            LoxErrorKind::LexerError(errors) => {
                for error in errors {
                    match error {
                        LexError::UnexpectedCharacter { lexeme } => {
                            let (line, col) = lexeme.find_in_source(&self.content);
                            writeln!(
                                f,
                                "Unexpected character \'{}\' at {line}:{col}",
                                lexeme.as_str(),
                            )?;
                        }
                        LexError::UnterminatedString => {
                            writeln!(f, "Missing closing delimiter \" for string")?;
                        }
                    }
                }
            }
            LoxErrorKind::ParseError(ParseError::UnexpectedToken(t)) => {
                let (line, col) = t.lexeme.find_in_source(&self.content);
                writeln!(
                    f,
                    "Unexpected token \"{}\" at {line}:{col}",
                    &t.lexeme.payload
                )?;
            }
        }
        Ok(())
    }
}

/// The `Lox` interpreter
pub struct Lox {}

impl Lox {
    pub fn new() -> Self {
        Self {}
    }

    pub fn eval<'a>(&mut self, source: &'a str) -> Result<(), LoxError<'a>> {
        let lexer = Lexer::new(source);
        let tokens = lexer.lex().map_err(|e| LoxError {
            content: source,
            kind: e.into(),
        })?;

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().map_err(|e| LoxError {
            content: source,
            kind: e.into(),
        })?;
        match evaluate(&ast) {
            Ok(value) => println!("{value}"),
            Err(e) => println!("{}", e.display(source)),
        }
        Ok(())
    }
}
