use miette::{SourceOffset, SourceSpan};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn spanned<T>(self, value: T) -> Spanned<T> {
        Spanned::new(value, self)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub const fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }
}

impl From<Span> for SourceSpan {
    fn from(value: Span) -> Self {
        Self::new(SourceOffset::from(value.start), value.end - value.start)
    }
}
