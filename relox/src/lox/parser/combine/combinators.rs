use crate::lox::{
    lexer::Token,
    parser::{PResult, ParserError},
};

use super::{Parse, StructuredLexer};

pub fn any<'a>(lexer: &mut StructuredLexer<'a>) -> PResult<'a, Token<'a>> {
    lexer
        .next()
        .transpose()
        .unwrap_or(Err(ParserError::UnexpectedEof))
}

pub fn opt<'a, O: 'a>(mut parser: impl Parse<'a, O>) -> impl for<'b> Parse<'a, Option<O>> {
    move |lexer: &mut StructuredLexer<'a>| match parser.parse_next(lexer) {
        Ok(out) => Ok(Some(out)),
        Err(ParserError::UnexpectedEof) => Ok(None),
        Err(e) => Err(e),
    }
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

pub(crate) use dispatch;
