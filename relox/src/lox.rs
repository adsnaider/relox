use std::fmt::Display;

use ast::ExprPrinter;
use derive_more::{Error, From};
use lexer::{LexError, Lexeme, Lexer};
use parser::{ParseError, Parser};

pub mod ast;
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

impl<'a> LoxError<'a> {
    fn find_lexeme(&self, lexeme: &Lexeme<'a>) -> (usize, usize) {
        let offset = lexeme.source_offset;
        let mut line = 0;
        let mut col = 0;
        self.content.chars().take(offset).for_each(|c| {
            if c == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        });
        (line, col)
    }
}

impl Display for LoxError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            LoxErrorKind::LexerError(errors) => {
                for error in errors {
                    match error {
                        LexError::UnexpectedCharacter { lexeme } => {
                            let (line, col) = self.find_lexeme(&lexeme);
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
                let (line, col) = self.find_lexeme(&t.lexeme);
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
        ExprPrinter.print(&ast);
        Ok(())
    }
}
