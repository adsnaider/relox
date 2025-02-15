use crate::lox::{
    lexer::{Token, TokenVariants},
    parser::{PResult, ParserError},
};

use super::{Parse, StructuredLexer};

pub fn any<'a>(lexer: &mut StructuredLexer<'a>) -> PResult<'a, Token<'a>> {
    lexer
        .next()
        .transpose()
        .unwrap_or(Err(ParserError::unexpected_eof(None, lexer.lexer.source())))
}

pub fn take_until<'a>(wants: TokenVariants) -> impl Parse<'a, Token<'a>> {
    move |lexer: &mut _| loop {
        let token = any.parse_next(lexer)?;
        if TokenVariants::from(&token.value) == wants {
            break Ok(token);
        }
    }
}

pub fn opt<'a, O: 'a, P>(mut parser: P) -> impl Parse<'a, Option<O>>
where
    P: Parse<'a, O>,
{
    move |lexer: &mut StructuredLexer<'a>| {
        log::debug!("Lexer EOF?: {}", lexer.is_eof());
        if lexer.is_eof() {
            Ok(None)
        } else {
            parser.parse_next(lexer).map(Some)
        }
    }
}

pub fn alt<'a, O: 'a, Alts: Alt<'a, O>>(mut alts: Alts) -> impl Parse<'a, O> {
    move |lexer: &mut StructuredLexer<'a>| alts.alt(lexer)
}

pub trait Alt<'a, O> {
    fn alt(&mut self, lexer: &mut StructuredLexer<'a>) -> PResult<'a, O>;
}

pub fn empty<'a>(_lexer: &mut StructuredLexer<'a>) -> PResult<'a, ()> {
    Ok(())
}

pub fn fail<'a, O>(_lexer: &mut StructuredLexer<'a>) -> PResult<'a, O> {
    Err(ParserError::Fail)
}

macro_rules! dispatch {
    ( $matcher:expr; $( $pattern:pat => $expr:expr )* ) => {
        move |i: &mut _| {
            let token = $matcher.parse_next(i)?;
            match token.value {
                $(
                    $pattern => $expr.parse_next(i),
                )*
            }
        }
    }
}

impl<'a, O: 'a, P1, P2> Alt<'a, O> for (P1, P2)
where
    P1: Parse<'a, O>,
    P2: Parse<'a, O>,
{
    fn alt(&mut self, lexer: &mut StructuredLexer<'a>) -> PResult<'a, O> {
        self.0
            .parse_next(lexer)
            .or_else(|_| self.1.parse_next(lexer))
    }
}

pub(crate) use dispatch;
