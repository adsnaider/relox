use crate::lox::{
    lexer::{Token, TokenValue},
    parser::{PResult, ParserError},
};

use super::{any, Parse, StructuredLexer};

pub fn any_num<'a>(lexer: &mut StructuredLexer<'a>) -> PResult<'a, (Token<'a>, f64)> {
    let token = any.parse_next(lexer)?;
    let TokenValue::Number(num) = &token.value else {
        return Err(ParserError::UnexpectedToken(token));
    };
    let num = *num;
    Ok((token, num))
}
