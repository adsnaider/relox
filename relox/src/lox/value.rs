use derive_more::derive::{
    Add, AddAssign, Display, Div, DivAssign, From, Mul, MulAssign, Neg, Sub, SubAssign,
};
use miette::Diagnostic;
use strum::EnumDiscriminants;
use thiserror::Error;

#[derive(Debug, Error, Clone, Copy, Diagnostic)]
#[error("Type error")]
#[diagnostic(help("Wanted: `{wants}` but got: `{got}`"))]
pub struct TypeError {
    wants: ValueDiscriminants,
    got: ValueDiscriminants,
}

#[derive(Debug, Copy, Clone, Display, From, EnumDiscriminants)]
pub enum Value {
    Num(Num),
    Bool(bool),
    #[display("null")]
    Nil,
}

impl core::fmt::Display for ValueDiscriminants {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = match self {
            ValueDiscriminants::Num => "number",
            ValueDiscriminants::Bool => "bool",
            ValueDiscriminants::Nil => "null",
        };
        write!(f, "{value}")
    }
}

impl Value {
    pub fn num(value: f64) -> Self {
        Self::Num(Num(value))
    }

    pub fn as_num(&self) -> Result<Num, TypeError> {
        match self {
            Value::Num(n) => Ok(*n),
            x => Err(TypeError {
                wants: ValueDiscriminants::Num,
                got: x.into(),
            }),
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
)]
#[mul(forward)]
#[div(forward)]
pub struct Num(pub f64);
