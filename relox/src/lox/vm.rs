use std::fmt::Display;

use thiserror::Error;

use crate::lox::chunk::Instr;

use super::{
    chunk::{Chunk, InvalidInstr},
    value::Value,
};

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error(transparent)]
    InvalidInstr(#[from] InvalidInstr),
    #[error(transparent)]
    StackError(#[from] StackError),
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
            let (_off, inst) = inst?;
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
                    self.stack.push(value)?;
                }
                Instr::Negate => {
                    let value = self.stack.pop().unwrap();
                    self.stack.push(-value)?;
                }
                Instr::Add => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a + b)?;
                }
                Instr::Sub => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a - b)?;
                }
                Instr::Mul => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a * b)?;
                }
                Instr::Div => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a / b)?;
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

#[derive(Debug, Error)]
pub enum StackError {
    #[error("Stack overflow")]
    StackOverflow(Value),
}

impl Stack {
    pub fn new(max_size: usize) -> Self {
        let mut stack = Vec::new();
        stack.reserve_exact(max_size);
        Self { stack, max_size }
    }

    pub fn push(&mut self, value: Value) -> Result<(), StackError> {
        if self.stack.len() >= self.max_size {
            Err(StackError::StackOverflow(value))
        } else {
            self.stack.push(value);
            Ok(())
        }
    }

    pub fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }
}
