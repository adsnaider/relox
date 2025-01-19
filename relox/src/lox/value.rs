use derive_more::derive::{
    Add, AddAssign, Display, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign,
};

#[derive(
    Debug, Copy, Clone, Display, Add, AddAssign, Sub, SubAssign, Mul, MulAssign, Div, DivAssign, Neg,
)]
#[mul(forward)]
#[div(forward)]
pub struct Value(pub f64);
