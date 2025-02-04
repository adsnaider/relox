use core::fmt::Display;

use derive_more::derive::{
    Add, AddAssign, Display, Div, DivAssign, From, Mul, MulAssign, Neg, Sub, SubAssign,
};
use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Error, Clone, Diagnostic)]
#[error("Type error")]
#[diagnostic(help("Wanted: {wants} but got: `{got}`"))]
pub struct TypeError {
    wants: AnyOfVec<ValueDiscriminants>,
    got: ValueDiscriminants,
}

impl TypeError {
    pub fn new(wants: Vec<ValueDiscriminants>, got: ValueDiscriminants) -> Self {
        Self {
            wants: AnyOfVec(wants),
            got,
        }
    }
}
#[derive(Debug, Clone)]
struct AnyOfVec<T>(Vec<T>);

impl<T: Display> Display for AnyOfVec<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.len() {
            0 => write!(f, "uninhabited"),
            1 => write!(f, "`{}`", self.0.first().unwrap()),
            _ => {
                write!(f, "One of (")?;
                write!(f, "`{}`", self.0.first().unwrap())?;
                for val in self.0.iter().skip(1) {
                    write!(f, ", `{}`", val)?;
                }
                write!(f, ")")?;
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone, Display, From)]
pub enum Value {
    Num(Num),
    Bool(bool),
    #[display("null")]
    Nil,
    Obj(Obj),
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::str(value)
    }
}
impl From<Box<String>> for Value {
    fn from(value: Box<String>) -> Self {
        Self::Obj(Obj {
            kind: ObjKind::Str(value),
        })
    }
}

#[derive(Debug, Clone, Display, From)]
pub struct Obj {
    pub kind: ObjKind,
}

#[derive(Debug, Clone, Display, From)]
pub enum ObjKind {
    Str(Box<String>),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Display)]
pub enum ValueDiscriminants {
    #[display("number")]
    Num,
    #[display("bool")]
    Bool,
    #[display("null")]
    Nil,
    #[display("string")]
    Str,
}

impl From<&Value> for ValueDiscriminants {
    fn from(value: &Value) -> Self {
        match value {
            Value::Num(_) => Self::Num,
            Value::Bool(_) => Self::Bool,
            Value::Nil => Self::Nil,
            Value::Obj(Obj {
                kind: ObjKind::Str(_),
            }) => Self::Str,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Num(a), Value::Num(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (
                Value::Obj(Obj {
                    kind: ObjKind::Str(a),
                }),
                Value::Obj(Obj {
                    kind: ObjKind::Str(b),
                }),
            ) => a == b,
            _ => false,
        }
    }
}

impl Value {
    pub fn num(value: f64) -> Self {
        Self::Num(Num(value))
    }

    pub fn str(value: String) -> Self {
        Self::Obj(Obj {
            kind: ObjKind::Str(Box::new(value.into())),
        })
    }

    pub fn as_num(&self) -> Result<Num, TypeError> {
        match self {
            Value::Num(n) => Ok(*n),
            x => Err(TypeError::new(vec![ValueDiscriminants::Num], x.into())),
        }
    }

    pub fn truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Nil => false,
            _ => true,
        }
    }
}

#[derive(
    Debug,
    Copy,
    Clone,
    Display,
    Add,
    AddAssign,
    Sub,
    SubAssign,
    Mul,
    MulAssign,
    Div,
    DivAssign,
    Neg,
    From,
    PartialEq,
    PartialOrd,
)]
#[mul(forward)]
#[div(forward)]
pub struct Num(pub f64);
