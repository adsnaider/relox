use std::fmt::{self, Display};

use super::{
    ast::Expr,
    lexer::{Token, TokenValue},
};

#[derive(Debug)]
pub enum Value {
    String(String),
    Number(f64),
    Bool(bool),
    Object(Object),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Number(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Object(o) => write!(f, "{o}"),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Object {}

impl Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[Object]")
    }
}

#[derive(Debug)]
pub struct RuntimeError<'a> {
    token: Token<'a>,
    msg: &'static str,
    kind: RErrorKind,
}

pub struct RichRuntimeError<'a, 'src> {
    error: &'a RuntimeError<'src>,
    source: &'src str,
}

impl Display for RichRuntimeError<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (row, col) = self.error.token.lexeme.find_in_source(self.source);
        write!(
            f,
            "{}: {} '{}' at {}:{}",
            self.error.kind, self.error.msg, self.error.token.lexeme.payload, row, col
        )
    }
}

impl<'src> RuntimeError<'src> {
    pub fn new(token: Token<'src>, msg: &'static str, kind: RErrorKind) -> Self {
        Self { token, msg, kind }
    }

    pub fn display<'a>(&'a self, src: &'src str) -> impl Display + 'a {
        RichRuntimeError {
            error: self,
            source: src,
        }
    }
}

#[derive(Debug)]
pub enum RErrorKind {
    TypeError,
}

impl Display for RErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RErrorKind::TypeError => write!(f, "TypeError"),
        }
    }
}

pub fn evaluate<'a>(expr: &Expr<'a>) -> Result<Value, RuntimeError<'a>> {
    match expr {
        Expr::Literal(l) => match &l.value.value {
            TokenValue::String(s) => Ok(Value::String(s.clone())),
            TokenValue::Number(n) => Ok(Value::Number(*n)),
            TokenValue::True => Ok(Value::Bool(true)),
            TokenValue::False => Ok(Value::Bool(false)),
            TokenValue::Nil => Ok(Value::Nil),
            other => unreachable!("Unexpected literal token: {other:?}"),
        },
        Expr::Unary(unary) => {
            let rhs = evaluate(&unary.rhs)?;
            match &unary.op.value {
                TokenValue::Minus => match rhs {
                    Value::Number(n) => Ok(Value::Number(-n)),
                    _ => Err(RuntimeError::new(
                        unary.op.clone(),
                        "Invalid operand for unary expression",
                        RErrorKind::TypeError,
                    )),
                },
                TokenValue::Bang => Ok(Value::Bool(!truthy(&rhs))),
                other => unreachable!("Unexpected token for unary expression: {other:?}"),
            }
        }
        Expr::Binary(binary) => {
            let lhs = evaluate(&binary.lhs)?;
            let rhs = evaluate(&binary.rhs)?;
            match &binary.op.value {
                TokenValue::Plus => match (lhs, rhs) {
                    (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs + rhs)),
                    (Value::String(lhs), Value::String(rhs)) => Ok(Value::String(lhs + &rhs)),
                    (_lhs, _rhs) => Err(RuntimeError::new(
                        binary.op.clone(),
                        "Invalid operands for binary expression",
                        RErrorKind::TypeError,
                    )),
                },
                TokenValue::Minus => match (lhs, rhs) {
                    (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs - rhs)),
                    (_lhs, _rhs) => Err(RuntimeError::new(
                        binary.op.clone(),
                        "Invalid operands for binary expression",
                        RErrorKind::TypeError,
                    )),
                },
                TokenValue::Star => match (lhs, rhs) {
                    (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs * rhs)),
                    (_lhs, _rhs) => Err(RuntimeError::new(
                        binary.op.clone(),
                        "Invalid operands for binary expression",
                        RErrorKind::TypeError,
                    )),
                },
                TokenValue::Slash => match (lhs, rhs) {
                    (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs / rhs)),
                    (_lhs, _rhs) => Err(RuntimeError::new(
                        binary.op.clone(),
                        "Invalid operands for binary expression",
                        RErrorKind::TypeError,
                    )),
                },
                TokenValue::Greater => match (lhs, rhs) {
                    (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Bool(lhs > rhs)),
                    (_lhs, _rhs) => Err(RuntimeError::new(
                        binary.op.clone(),
                        "Invalid operands for binary expression",
                        RErrorKind::TypeError,
                    )),
                },
                TokenValue::GreaterEqual => match (lhs, rhs) {
                    (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Bool(lhs >= rhs)),
                    (_lhs, _rhs) => Err(RuntimeError::new(
                        binary.op.clone(),
                        "Invalid operands for binary expression",
                        RErrorKind::TypeError,
                    )),
                },
                TokenValue::Less => match (lhs, rhs) {
                    (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Bool(lhs < rhs)),
                    (_lhs, _rhs) => Err(RuntimeError::new(
                        binary.op.clone(),
                        "Invalid operands for binary expression",
                        RErrorKind::TypeError,
                    )),
                },
                TokenValue::LessEqual => match (lhs, rhs) {
                    (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Bool(lhs <= rhs)),
                    (_lhs, _rhs) => Err(RuntimeError::new(
                        binary.op.clone(),
                        "Invalid operands for binary expression",
                        RErrorKind::TypeError,
                    )),
                },
                TokenValue::EqualEqual => Ok(Value::Bool(equals(&lhs, &rhs))),
                TokenValue::BangEqual => Ok(Value::Bool(!equals(&lhs, &rhs))),
                other => unreachable!("Unexpected token for binary expression: {other:?}"),
            }
        }
        Expr::Grouping(expr) => evaluate(&expr.expr),
    }
}

fn equals(lhs: &Value, rhs: &Value) -> bool {
    match (lhs, rhs) {
        (Value::Number(lhs), Value::Number(rhs)) => lhs == rhs,
        (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
        (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
        (Value::Object(lhs), Value::Object(rhs)) => lhs == rhs,
        (Value::Nil, Value::Nil) => true,
        (_, Value::Nil) => false,
        (Value::Nil, _) => false,
        _ => panic!("Type error: Invalid use of '=='"),
    }
}

fn truthy(value: &Value) -> bool {
    match value {
        Value::Bool(b) => *b,
        Value::Nil => false,
        _ => true,
    }
}
