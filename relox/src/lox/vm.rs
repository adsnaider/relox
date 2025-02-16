pub mod gc;

use std::fmt::Display;

use derive_more::derive::{Display, Error, From};
use gc::Heap;
use hashbrown::HashMap;
use miette::Diagnostic;

use crate::lox::{chunk::Instr, value::ValueDiscriminants};

use super::{
    chunk::{Chunk, ConstValue, InvalidInstr},
    compiler::GlobalId,
    value::{TypeError, Value},
};

#[derive(Debug, From, Error, Display, Diagnostic)]
pub enum RErrorKind {
    InvalidInstr(#[from] InvalidInstr),
    StackError(#[from] StackError),
    #[diagnostic(transparent)]
    TypeError(#[from] TypeError),
    #[display("Undefined reference to global: {_0}")]
    UndefinedGlobal(#[error(not(source))] GlobalId),
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
    heap: Heap,
    globals: HashMap<GlobalId, Value>,
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

impl Drop for Vm {
    fn drop(&mut self) {
        unsafe { self.heap.drain() };
    }
}

impl Vm {
    pub fn new() -> Self {
        Self::new_with_stack_size(256)
    }

    pub fn new_with_stack_size(max_stack_size: usize) -> Self {
        Self {
            stack: Stack::new(max_stack_size),
            heap: Heap::new(),
            globals: HashMap::new(),
        }
    }

    fn const_to_runtime(&mut self, value: ConstValue) -> Value {
        match value {
            ConstValue::Num(n) => Value::num(n),
            ConstValue::Str(s) => {
                let str = self.heap.push_interned(s.as_str());
                Value::str(str)
            }
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
                    let _value = self.stack.pop().unwrap();
                    todo!();
                }
                Instr::Const(const_idx) => {
                    let value = chunk.get_constant(const_idx).unwrap();
                    let value = self.const_to_runtime(value);
                    self.stack.push(value).add_ctx(off)?;
                }
                Instr::Negate => {
                    let value = self.stack.pop().unwrap().as_num().add_ctx(off)?;
                    self.stack.push(-value).add_ctx(off)?;
                }
                Instr::Add => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    match (a, b) {
                        (Value::Num(a), Value::Num(b)) => self.stack.push(a + b).add_ctx(off)?,
                        (Value::Str(a), Value::Str(b)) => {
                            let value = String::from_iter([a.data(), b.data()]);
                            let string = self.heap.push_interned(value.as_str());
                            self.stack.push(string).add_ctx(off)?;
                        }
                        (Value::Num(_), ref b) => {
                            Err(RErrorKind::TypeError(TypeError::new(
                                vec![ValueDiscriminants::Num],
                                b.into(),
                            )))
                            .add_ctx(off)?;
                        }
                        (Value::Str(_), ref b) => {
                            Err(RErrorKind::TypeError(TypeError::new(
                                vec![ValueDiscriminants::Str],
                                b.into(),
                            )))
                            .add_ctx(off)?;
                        }
                        (ref v, _) => {
                            Err(RErrorKind::TypeError(TypeError::new(
                                vec![ValueDiscriminants::Num, ValueDiscriminants::Str],
                                v.into(),
                            )))
                            .add_ctx(off)?;
                        }
                    }
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
                Instr::Eq => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a == b).add_ctx(off)?;
                }
                Instr::Less => {
                    let b = self.stack.pop().unwrap().as_num().add_ctx(off)?;
                    let a = self.stack.pop().unwrap().as_num().add_ctx(off)?;
                    self.stack.push(a < b).add_ctx(off)?;
                }
                Instr::Greater => {
                    let b = self.stack.pop().unwrap().as_num().add_ctx(off)?;
                    let a = self.stack.pop().unwrap().as_num().add_ctx(off)?;
                    self.stack.push(a > b).add_ctx(off)?;
                }
                Instr::Print => {
                    let value = self.stack.pop().unwrap();
                    println!("{value}");
                }
                Instr::Pop => {
                    self.stack.pop().unwrap();
                }
                Instr::DefineGlobal(global_id) => {
                    let value = self.stack.pop().unwrap();
                    let _ = self.globals.insert(global_id, value);
                }
                Instr::GetGlobal(global_id) => {
                    let value = self
                        .globals
                        .get(&global_id)
                        .ok_or(RErrorKind::UndefinedGlobal(global_id))
                        .add_ctx(off)?
                        .clone();
                    self.stack.push(value).add_ctx(off)?;
                }
                Instr::SetGlobal(global_id) => {
                    let value = self.stack.pop().unwrap();
                    *self
                        .globals
                        .get_mut(&global_id)
                        .ok_or(RErrorKind::UndefinedGlobal(global_id))
                        .add_ctx(off)? = value;
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
