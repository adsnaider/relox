use std::fmt::Display;

use derive_more::derive::{Display, Error, From};
use miette::Diagnostic;

use crate::lox::chunk::Instr;

use super::{
    chunk::{Chunk, InvalidInstr},
    value::{TypeError, Value},
};

#[derive(Debug, From, Error, Display, Diagnostic)]
pub enum RErrorKind {
    InvalidInstr(#[from] InvalidInstr),
    StackError(#[from] StackError),
    #[diagnostic(transparent)]
    TypeError(#[from] TypeError),
}

#[derive(Debug, Error, Display, Diagnostic)]
#[diagnostic(forward(kind))]
#[display("{kind}")]
pub struct RuntimeError {
    kind: RErrorKind,
    #[label]
    offset: usize,
}

pub type IResult = Result<(), RuntimeError>;

/// The Lox language virtual machine / bytecode executor
#[derive(Debug)]
pub struct Vm {
    stack: Stack,
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

trait RuntimeErrorCtx<T> {
    fn add_ctx(self, offset: usize) -> Result<T, RuntimeError>;
}

impl<T, E> RuntimeErrorCtx<T> for Result<T, E>
where
    E: Into<RErrorKind>,
{
    fn add_ctx(self, offset: usize) -> Result<T, RuntimeError> {
        self.map_err(|e| RuntimeError {
            kind: e.into(),
            offset,
        })
    }
}

impl Vm {
    pub fn new() -> Self {
        Self::new_with_stack_size(256)
    }

    pub fn new_with_stack_size(max_stack_size: usize) -> Self {
        Self {
            stack: Stack::new(max_stack_size),
        }
    }

    pub fn interpret(&mut self, chunk: Chunk) -> IResult {
        for inst in chunk.instructions() {
            let (off, inst) = inst.add_ctx(0)?;
            log::debug!("instruction: {inst:?}");
            log::trace!(
                "\n========== STACK ===========\n{}\n============================",
                self.stack
            );
            match inst {
                Instr::Return => {
                    let value = self.stack.pop().unwrap();
                    println!("{value}");
                }
                Instr::Const(const_idx) => {
                    let value = chunk.get_constant(const_idx).unwrap();
                    self.stack.push(value).add_ctx(off)?;
                }
                Instr::Negate => {
                    let value = self.stack.pop().unwrap().as_num().add_ctx(off)?;
                    self.stack.push(-value).add_ctx(off)?;
                }
                Instr::Add => {
                    let b = self.stack.pop().unwrap().as_num().add_ctx(off)?;
                    let a = self.stack.pop().unwrap().as_num().add_ctx(off)?;
                    self.stack.push(a + b).add_ctx(off)?;
                }
                Instr::Sub => {
                    let b = self.stack.pop().unwrap().as_num().add_ctx(off)?;
                    let a = self.stack.pop().unwrap().as_num().add_ctx(off)?;
                    self.stack.push(a - b).add_ctx(off)?;
                }
                Instr::Mul => {
                    let b = self.stack.pop().unwrap().as_num().add_ctx(off)?;
                    let a = self.stack.pop().unwrap().as_num().add_ctx(off)?;
                    self.stack.push(a * b).add_ctx(off)?;
                }
                Instr::Div => {
                    let b = self.stack.pop().unwrap().as_num().add_ctx(off)?;
                    let a = self.stack.pop().unwrap().as_num().add_ctx(off)?;
                    self.stack.push(a / b).add_ctx(off)?;
                }
                Instr::True => {
                    self.stack.push(true).add_ctx(off)?;
                }
                Instr::False => {
                    self.stack.push(false).add_ctx(off)?;
                }
                Instr::Nil => {
                    self.stack.push(Value::Nil).add_ctx(off)?;
                }
                Instr::Not => {
                    let a = self.stack.pop().unwrap().truthy();
                    self.stack.push(!a).add_ctx(off)?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Stack {
    stack: Vec<Value>,
    max_size: usize,
}

impl Display for Stack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "size: {}, max: {}", self.stack.len(), self.max_size)?;
        f.write_str("[")?;
        if let Some(first) = self.stack.first() {
            write!(f, "{}", first)?;
            for value in self.stack.iter().skip(1) {
                write!(f, ", {value}")?;
            }
        }
        f.write_str("]")?;
        Ok(())
    }
}

#[derive(Debug, Error, Display)]
pub enum StackError {
    #[display("Stack overflow")]
    StackOverflow(#[error(not(source))] Value),
}

impl Stack {
    pub fn new(max_size: usize) -> Self {
        let mut stack = Vec::new();
        stack.reserve_exact(max_size);
        Self { stack, max_size }
    }

    pub fn push(&mut self, value: impl Into<Value>) -> Result<(), StackError> {
        if self.stack.len() >= self.max_size {
            Err(StackError::StackOverflow(value.into()))
        } else {
            self.stack.push(value.into());
            Ok(())
        }
    }

    pub fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }
}
