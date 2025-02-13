use derive_more::Display;
use thiserror::Error;

#[repr(transparent)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Display)]
pub struct ConstIdx(u8);

#[derive(Debug, Copy, Clone, Eq, PartialEq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Instr {
    Return = 0,
    Const(ConstIdx) = 1,
    Negate = 2,
    Add = 3,
    Sub = 4,
    Mul = 5,
    Div = 6,
    True = 7,
    False = 8,
    Nil = 9,
    Not = 10,
    Eq = 11,
    Less = 12,
    Greater = 13,
}

#[derive(Error, Debug)]
pub enum InvalidInstr {
    #[error("Got EOF while parsing instruction")]
    UnexpectedEof,
    #[error("Unexpected instruction OP Code while parsing: {0}")]
    UnknownOpCode(u8),
}

impl Instr {
    pub fn try_from_code(code: &[u8]) -> Result<(Self, usize), InvalidInstr> {
        let disc = code.get(0).ok_or(InvalidInstr::UnexpectedEof)?;
        match *disc {
            0 => Ok((Self::Return, 1)),
            1 => {
                let idx = code.get(1).ok_or(InvalidInstr::UnexpectedEof)?;
                let idx = ConstIdx(*idx);
                Ok((Self::Const(idx), 2))
            }
            2 => Ok((Self::Negate, 1)),
            3 => Ok((Self::Add, 1)),
            4 => Ok((Self::Sub, 1)),
            5 => Ok((Self::Mul, 1)),
            6 => Ok((Self::Div, 1)),
            7 => Ok((Self::True, 1)),
            8 => Ok((Self::False, 1)),
            9 => Ok((Self::Nil, 1)),
            10 => Ok((Self::Not, 1)),
            11 => Ok((Self::Eq, 1)),
            12 => Ok((Self::Less, 1)),
            13 => Ok((Self::Greater, 1)),
            op => Err(InvalidInstr::UnknownOpCode(op)),
        }
    }

    pub fn serialize(&self, output: &mut Vec<u8>) {
        match self {
            Instr::Return => output.push(0),
            Instr::Const(idx) => {
                output.extend_from_slice(&[1, idx.0]);
            }
            Instr::Negate => output.push(2),
            Instr::Add => output.push(3),
            Instr::Sub => output.push(4),
            Instr::Mul => output.push(5),
            Instr::Div => output.push(6),
            Instr::True => output.push(7),
            Instr::False => output.push(8),
            Instr::Nil => output.push(9),
            Instr::Not => output.push(10),
            Instr::Eq => output.push(11),
            Instr::Less => output.push(12),
            Instr::Greater => output.push(13),
        }
    }

    pub fn disassemble(&self, consts: &Constants) -> String {
        match self {
            Instr::Return => format!("<ret>"),
            Instr::Const(const_idx) => match consts.get(const_idx) {
                Some(value) => format!("<const> [{const_idx}] '{value}'"),
                None => format!("<const> [{const_idx}] undefined"),
            },
            Instr::Negate => format!("<neg>"),
            Instr::Add => format!("<add>"),
            Instr::Sub => format!("<sub>"),
            Instr::Mul => format!("<mul>"),
            Instr::Div => format!("<div>"),
            Instr::True => format!("<true>"),
            Instr::False => format!("<false>"),
            Instr::Nil => format!("<null>"),
            Instr::Not => format!("<not>"),
            Instr::Eq => format!("<eq>"),
            Instr::Less => format!("<less>"),
            Instr::Greater => format!("<greater>"),
        }
    }
}

#[derive(Debug, Clone, Display)]
pub enum ConstValue {
    Num(f64),
    Str(String),
}

#[derive(Debug, Default)]
pub struct Constants(Vec<ConstValue>);

impl Constants {
    pub fn get(&self, idx: &ConstIdx) -> Option<ConstValue> {
        self.0.get(idx.0 as usize).cloned()
    }

    pub fn push(&mut self, constant: ConstValue) -> ConstIdx {
        self.0.push(constant);
        ConstIdx((self.0.len() - 1).try_into().unwrap())
    }
}

#[derive(Debug, Default)]
pub struct Code {
    bytecode: Vec<u8>,
    lines: Vec<usize>,
}

impl Code {
    pub fn add_instruction(&mut self, inst: Instr, line: usize) {
        let start_len = self.bytecode.len();
        inst.serialize(&mut self.bytecode);
        let count = self.bytecode.len() - start_len;
        self.lines.extend(std::iter::repeat(line).take(count));
    }

    pub fn instructions(&self) -> ChunkIter<'_> {
        ChunkIter {
            code: &self.bytecode,
            offset: 0,
        }
    }
}

#[derive(Debug, Default)]
pub struct Chunk {
    code: Code,
    consts: Constants,
}

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_instruction(&mut self, inst: Instr, line: usize) {
        self.code.add_instruction(inst, line);
    }

    pub fn add_constant(&mut self, constant: ConstValue) -> ConstIdx {
        self.consts.push(constant)
    }

    pub fn get_constant(&self, idx: ConstIdx) -> Option<ConstValue> {
        self.consts.get(&idx)
    }

    pub fn instructions(&self) -> ChunkIter<'_> {
        self.code.instructions()
    }

    pub fn disassemble(&self) -> String {
        let mut output = String::new();
        for (off, inst) in self.instructions().map(Result::unwrap) {
            let line = self.code.lines[off];
            let indicator = if off > 0 && line == self.code.lines[off - 1] {
                format!("|")
            } else {
                format!("{line}")
            };
            output.push_str(&format!(
                "{off:#06X}  {:4} {}\n",
                indicator,
                inst.disassemble(&self.consts),
            ));
        }
        output
    }
}

pub struct ChunkIter<'a> {
    code: &'a [u8],
    offset: usize,
}

impl Iterator for ChunkIter<'_> {
    type Item = Result<(usize, Instr), InvalidInstr>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.offset >= self.code.len() {
            return None;
        }
        let out = Instr::try_from_code(&self.code[self.offset..]);
        match out {
            Ok((inst, count)) => {
                let off = self.offset;
                self.offset += count;
                Some(Ok((off, inst)))
            }
            Err(e) => Some(Err(e)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dissasembly() {
        let mut chunk = Chunk::new();
        chunk.add_instruction(Instr::Return, 123);
        let idx = chunk.add_constant(ConstValue::Num(10.2));
        chunk.add_instruction(Instr::Const(idx), 123);
        chunk.add_instruction(Instr::Return, 124);
        let disassembled = chunk.disassemble();
        let disassembled: Vec<_> = disassembled.lines().collect();
        assert_eq!(disassembled[0], "0x0000  123  <ret>");
        assert_eq!(disassembled[1], "0x0001  |    <const> [0] '10.2'");
        assert_eq!(disassembled[2], "0x0003  124  <ret>");
    }
}
