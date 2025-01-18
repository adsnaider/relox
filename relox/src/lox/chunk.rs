use derive_more::Display;
use thiserror::Error;

#[derive(Debug, Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Display)]
pub enum Instr {
    #[display("<ret>")]
    Return = 0,
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
            op => Err(InvalidInstr::UnknownOpCode(op)),
        }
    }

    pub fn serialize(&self, output: &mut Vec<u8>) {
        match self {
            Instr::Return => output.push(0),
        }
    }

    pub fn disassemble(&self) -> String {
        format!("{self}")
    }
}

#[derive(Debug, Default)]
pub struct Chunk {
    code: Vec<u8>,
}

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_instruction(&mut self, inst: Instr) {
        inst.serialize(&mut self.code);
    }

    pub fn instructions(&self) -> ChunkIter<'_> {
        ChunkIter {
            code: &self.code,
            offset: 0,
        }
    }

    pub fn disassemble(&self) -> String {
        let mut output = String::new();
        for (off, inst) in self.instructions().map(Result::unwrap) {
            output.push_str(&format!("{off:#06X}   {inst}\n"));
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
        chunk.add_instruction(Instr::Return);
        chunk.add_instruction(Instr::Return);
        chunk.add_instruction(Instr::Return);
        let disassembled = chunk.disassemble();
        let disassembled: Vec<_> = disassembled.lines().collect();
        assert_eq!(disassembled[0], "0x0000   <ret>");
        assert_eq!(disassembled[1], "0x0001   <ret>");
        assert_eq!(disassembled[2], "0x0002   <ret>");
    }
}
