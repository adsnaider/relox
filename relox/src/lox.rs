use std::fmt::Display;

use derive_more::{Error, From};
use interpreter::Interpreter;
use lexer::{LexError, Lexer, TokenValue};
use parser::{ParseError, Parser};

pub mod ast;
pub mod interpreter;
pub mod introspect;
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
            LoxErrorKind::ParseError(ParseError::UnexpectedToken(t)) => match t.value {
                TokenValue::Eof => {
                    writeln!(f, "Unexpected end of file")?;
                }
                _ => {
                    let (line, col) = t.lexeme.find_in_source(&self.content);
                    writeln!(
                        f,
                        "Unexpected token \"{}\" at {line}:{col}",
                        &t.lexeme.payload
                    )?;
                }
            },
            LoxErrorKind::ParseError(ParseError::InvalidAssignment(t)) => match t.value {
                TokenValue::Eof => {
                    writeln!(f, "Unexpected end of file")?;
                }
                _ => {
                    let (line, col) = t.lexeme.find_in_source(&self.content);
                    writeln!(
                        f,
                        "Invalid assignment at \"{}\" at {line}:{col}",
                        &t.lexeme.payload
                    )?;
                }
            },
        }
        Ok(())
    }
}

/// The `Lox` interpreter
pub struct Lox {
    interpreter: Interpreter,
}

impl Lox {
    pub fn new() -> Self {
        Self {
            interpreter: Interpreter::new(),
        }
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
        match self.interpreter.evaluate(&ast) {
            Ok(value) => print!("{value}"),
            Err(e) => println!("{}", e.display(source)),
        }
        Ok(())
    }
}
