use derive_more::derive::Display;
use strum::EnumDiscriminants;
use thiserror::Error;

/// Lexer for the Lox language
pub struct Lexer<'a> {
    input: &'a str,
    offset: usize,
}

struct Eof;

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input, offset: 0 }
    }

    pub fn lex(self) -> Result<Vec<Token<'a>>, Vec<LexError<'a>>> {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();
        let mut is_last_error = false;
        for output in self {
            match output {
                Ok(t) => {
                    tokens.push(t);
                    is_last_error = false;
                }
                Err(e) => {
                    if !is_last_error {
                        errors.push(e);
                    }
                    is_last_error = true;
                }
            }
        }
        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }

    fn take(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.offset += c.len_utf8();
        Some(c)
    }

    fn take_while<F: FnMut(char) -> bool>(&mut self, mut matcher: F) -> Result<(), Eof> {
        while self.take_if(&mut matcher)? {}
        Ok(())
    }

    fn take_until<F: FnMut(char) -> bool>(&mut self, mut matcher: F) -> Result<(), Eof> {
        self.take_while(|c| !matcher(c))?;
        self.take().ok_or(Eof)?;
        Ok(())
    }

    fn take_equals(&mut self, c: char) -> Result<bool, Eof> {
        self.take_if(|next| next == c)
    }

    fn take_if<F: FnOnce(char) -> bool>(&mut self, matcher: F) -> Result<bool, Eof> {
        let c = self.peek().ok_or(Eof)?;
        if matcher(c) {
            self.offset += c.len_utf8();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn peek(&self) -> Option<char> {
        self.peek_nth(0)
    }

    fn peek_nth(&self, count: usize) -> Option<char> {
        self.input.get(self.offset..)?.chars().skip(count).next()
    }

    fn last_lexeme(&self, start_offset: usize) -> Lexeme<'a> {
        Lexeme::new(&self.input, Span::new(start_offset, self.offset))
    }
}
fn is_alpha(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_alphanumeric(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

#[derive(Debug, Error, Clone)]
pub enum LexError<'a> {
    #[error("Unexpected character: {lexeme}")]
    UnexpectedCharacter { lexeme: Lexeme<'a> },
    #[error("Unterminated string")]
    UnterminatedString,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let (start_offset, value) = loop {
            let start_offset = self.offset;
            let c = self.take()?;
            let token = match c {
                '(' => TokenValue::LeftParen,
                ')' => TokenValue::RightParen,
                '{' => TokenValue::LeftBrace,
                '}' => TokenValue::RightBrace,
                ',' => TokenValue::Comma,
                '.' => TokenValue::Dot,
                '-' => TokenValue::Minus,
                '+' => TokenValue::Plus,
                ';' => TokenValue::Semicolon,
                '*' => TokenValue::Star,
                '!' if self.take_equals('=').unwrap_or(false) => TokenValue::BangEqual,
                '!' => TokenValue::Bang,
                '=' if self.take_equals('=').unwrap_or(false) => TokenValue::EqualEqual,
                '=' => TokenValue::Equal,
                '<' if self.take_equals('=').unwrap_or(false) => TokenValue::LessEqual,
                '<' => TokenValue::Less,
                '>' if self.take_equals('=').unwrap_or(false) => TokenValue::GreaterEqual,
                '>' => TokenValue::Greater,
                '/' if self.take_equals('/').unwrap_or(false) => {
                    // Skip the comments
                    let _ = self.take_while(|c| c != '\n');
                    continue;
                }
                '/' => TokenValue::Slash,
                '"' => match self.take_until(|c| c == '"') {
                    Ok(()) => {
                        let lexeme = self.last_lexeme(start_offset);
                        let lit = lexeme.string_literal();
                        TokenValue::String(lit)
                    }
                    Err(Eof) => return Some(Err(LexError::UnterminatedString)),
                },
                c if c.is_digit(10) => {
                    let _ = self.take_while(|c| c.is_digit(10));
                    match (self.peek_nth(0), self.peek_nth(1)) {
                        (Some('.'), Some(c)) if c.is_digit(10) => {
                            self.take();
                            let _ = self.take_while(|c| c.is_digit(10));
                        }
                        _ => {}
                    }
                    let lexeme = self.last_lexeme(start_offset);
                    let lit = lexeme.number_literal();
                    TokenValue::Number(lit)
                }
                c if is_alpha(c) => {
                    let _ = self.take_while(|c| is_alphanumeric(c));
                    let lexeme = self.last_lexeme(start_offset);
                    let word = lexeme.identifier();
                    reserved_keywords(word.as_bytes()).unwrap_or(TokenValue::Ident(word))
                }
                c if c.is_ascii_whitespace() => {
                    continue;
                }
                _ => {
                    return Some(Err(LexError::UnexpectedCharacter {
                        lexeme: self.last_lexeme(start_offset),
                    }))
                }
            };
            break (start_offset, token);
        };
        Some(Ok(Token {
            value,
            lexeme: self.last_lexeme(start_offset),
        }))
    }
}

const fn reserved_keywords(keyword: &[u8]) -> Option<TokenValue> {
    match keyword {
        b"and" => Some(TokenValue::And),
        b"class" => Some(TokenValue::Class),
        b"else" => Some(TokenValue::Else),
        b"false" => Some(TokenValue::False),
        b"for" => Some(TokenValue::For),
        b"fun" => Some(TokenValue::Fun),
        b"if" => Some(TokenValue::If),
        b"nil" => Some(TokenValue::Nil),
        b"or" => Some(TokenValue::Or),
        b"print" => Some(TokenValue::Print),
        b"return" => Some(TokenValue::Return),
        b"super" => Some(TokenValue::Super),
        b"this" => Some(TokenValue::This),
        b"true" => Some(TokenValue::True),
        b"var" => Some(TokenValue::Var),
        b"while" => Some(TokenValue::While),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Debug, Clone, Display)]
#[display("'{}'", self.payload())]
pub struct Lexeme<'a> {
    pub span: Span,
    pub source: &'a str,
}

impl<'a> Lexeme<'a> {
    pub fn as_str(&self) -> &'a str {
        self.payload()
    }

    pub fn payload(&self) -> &'a str {
        &self.source[self.span.start..self.span.end]
    }

    pub fn new(source: &'a str, span: Span) -> Self {
        Self { source, span }
    }

    pub fn locate(&self) -> (usize, usize) {
        let mut line = 1;
        let mut col = 1;
        let mut offset = 0;
        self.source
            .chars()
            .take_while(|c| {
                let current_offset = offset;
                offset += c.len_utf8();
                current_offset <= self.span.start
            })
            .for_each(|c| {
                if c == '\n' {
                    line += 1;
                    col = 1;
                } else {
                    col += 1;
                }
            });
        (line, col)
    }

    fn string_literal(&self) -> String {
        self.payload().trim_matches('"').to_string()
    }

    fn number_literal(&self) -> f64 {
        self.payload()
            .parse()
            .expect("Caller guarantees this can be parsed as an f64")
    }

    fn identifier(&self) -> String {
        self.payload().to_string()
    }
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub value: TokenValue,
    pub lexeme: Lexeme<'a>,
}

#[derive(Debug, Clone, EnumDiscriminants)]
#[strum_discriminants(name(TokenVariants))]
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
    Eof,
}
