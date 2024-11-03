use std::borrow::Cow;

/// Lexer for the Lox language
pub struct Lexer<'a> {
    input: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct Lexeme<'a> {
    payload: Cow<'a, str>,
    file_offset: usize,
}

pub struct Token<'a> {
    value: TokenValue,
    lexeme: Lexeme<'a>,
}

#[derive(Debug, Clone)]
pub enum TokenValue {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Ident(String),
    String(String),
    Number(f64),
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}
