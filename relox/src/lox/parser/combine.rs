#![allow(unused)]
mod combinators;
pub use combinators::*;

mod literals;
use derive_more::derive::{Deref, DerefMut};
pub use literals::*;

use crate::lox::{
    ast::span::Span,
    lexer::{LexError, Lexer, Token, TokenValue, TokenVariants},
};

use super::{PResult, ParserError};

#[derive(Debug)]
pub struct StructuredLexer<'a> {
    lexer: Lexer<'a>,
}

#[derive(Debug, Clone)]
pub struct Checkpoint<'a>(Lexer<'a>);

pub trait Parse<'a, O: 'a> {
    fn parse_next(&mut self, lexer: &mut StructuredLexer<'a>) -> PResult<'a, O>;
    fn peek_next(&mut self, lexer: &mut StructuredLexer<'a>) -> PResult<'a, Peeked<'a, O>> {
        let original_state = lexer.checkpoint();
        let next = self.parse_next(lexer);
        let current_state = lexer.checkpoint();
        lexer.restore(original_state);
        Ok(Peeked {
            value: next?,
            state: current_state,
        })
    }
}

#[derive(Deref, DerefMut)]
pub struct Peeked<'a, T> {
    #[deref]
    #[deref_mut]
    value: T,
    state: Checkpoint<'a>,
}

impl<'a, T> Peeked<'a, T> {
    pub fn consume(self, lexer: &mut StructuredLexer<'a>) -> T {
        lexer.restore(self.state);
        self.value
    }
}

impl<'a, T> Peeked<'a, Option<T>> {
    pub fn transpose(self) -> Option<Peeked<'a, T>> {
        match self.value {
            Some(value) => Some(Peeked {
                value,
                state: self.state,
            }),
            None => None,
        }
    }
}

impl<'a, F, O: 'a> Parse<'a, O> for F
where
    F: FnMut(&mut StructuredLexer<'a>) -> PResult<'a, O>,
{
    fn parse_next(&mut self, lexer: &mut StructuredLexer<'a>) -> PResult<'a, O> {
        self(lexer)
    }
}

impl<'a> Parse<'a, Token<'a>> for TokenValue {
    fn parse_next(&mut self, lexer: &mut StructuredLexer<'a>) -> PResult<'a, Token<'a>> {
        let next = any.parse_next(lexer)?;
        if next.value == *self {
            Ok(next)
        } else {
            Err(ParserError::UnexpectedToken(next))
        }
    }
}

impl<'a> Parse<'a, Token<'a>> for TokenVariants {
    fn parse_next(&mut self, lexer: &mut StructuredLexer<'a>) -> PResult<'a, Token<'a>> {
        let next = any.parse_next(lexer)?;
        if *self == (&next.value).into() {
            Ok(next)
        } else {
            Err(ParserError::UnexpectedToken(next))
        }
    }
}

impl<'a> StructuredLexer<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self { lexer }
    }

    pub fn checkpoint(&self) -> Checkpoint<'a> {
        Checkpoint(self.lexer.clone())
    }

    pub fn restore(&mut self, checkpoint: Checkpoint<'a>) {
        log::debug!("Restoring lexer checkpoint");
        self.lexer = checkpoint.0;
    }

    pub fn next(&mut self) -> PResult<'a, Option<Token<'a>>> {
        let next = self.lexer.next();
        log::debug!("Lexing. Next up: {next:?}");
        match next {
            Some(Ok(t)) => Ok(Some(t)),
            Some(Err(e)) => Err(self.lex_panic(e)),
            None => Ok(None),
        }
    }

    pub fn enter_span(&mut self) -> EnterSpan {
        EnterSpan {
            start: self.offset(),
        }
    }

    fn lex_panic(&mut self, e: LexError<'a>) -> ParserError<'a> {
        let mut errors = vec![e];
        errors.extend(self.lexer.drain().filter_map(|t| t.err()));
        ParserError::LexerError(errors)
    }

    pub fn offset(&self) -> usize {
        self.lexer.offset()
    }
}

pub struct EnterSpan {
    start: usize,
}

impl EnterSpan {
    pub fn end(&self, lexer: &StructuredLexer<'_>) -> Span {
        Span {
            start: self.start,
            end: lexer.offset(),
        }
    }
}
