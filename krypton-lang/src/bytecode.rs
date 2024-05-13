use std::fmt::{Debug, Display, Formatter};

use crate::bytecode::Error::UnrecognizedOpcode;
use crate::bytecode::Instruction::Return;

#[derive(Debug, Clone)]
pub enum Error {
    UnrecognizedOpcode(String),
    EmptyInstructionBytes,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnrecognizedOpcode(op) => format!("Unrecognized opcode: {}", op),
                Error::EmptyInstructionBytes => "Cannot decode instruction from empty bytes".to_string(),
            }
        )
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    Return,
}

impl Instruction {
    pub fn assemble(instruction: &str) -> Result<Self> {
        match instruction {
            "ret" => Ok(Return),
            _ => Err(UnrecognizedOpcode(instruction.to_string())),
        }
    }

    pub fn disassemble(&self) -> String {
        match self {
            Return => "ret".to_string(),
        }
    }

    pub fn opcode(&self) -> u8 {
        match self {
            Return => 0b_0000_0000,
        }
    }

    #[allow(clippy::vec_init_then_push)]
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(1);
        bytes.push(self.opcode());
        // match self {
        //     Return => bytes.push(0b_0000_0000),
        // }
        bytes
    }

    pub fn from_bytes(bytes: &[u8], offset: &mut usize) -> Result<Self> {
        if bytes.is_empty() {
            return Err(Error::EmptyInstructionBytes);
        }
        let (instruction, size) = match bytes[0] {
            0b_0000_0000 => (Ok(Return), 1),
            byte => (Err(UnrecognizedOpcode(byte.to_string())), 0),
        };
        *offset += size;
        instruction
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:04} {}", self.opcode(), self.disassemble())
    }
}

#[derive(Debug, Clone, Default)]
pub struct Bytecode {
    instructions: Vec<Instruction>,
}

impl Bytecode {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }

    pub fn push(&mut self, op: Instruction) {
        self.instructions.push(op);
    }

    pub fn get_instruction(&self, index: usize) -> Option<&Instruction> {
        self.instructions.get(index)
    }

    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    pub fn is_empty(&self) -> bool {
        self.instructions.is_empty()
    }

    pub fn assemble(assembly: &[&str]) -> Result<Self> {
        let mut bytecode = Bytecode::new();
        for instruction in assembly {
            bytecode.push(Instruction::assemble(instruction)?);
        }
        Ok(bytecode)
    }

    pub fn assemble_from_string(assembly: &str) -> Result<Self> {
        let instructions: Vec<&str> = assembly
            .split("\r\n")
            .flat_map(|asm| asm.split('\n'))
            .filter(|asm| !asm.is_empty())
            .collect();
        Self::assemble(&instructions)
    }

    pub fn disassemble(&self) -> Vec<String> {
        self.instructions.iter().map(|op| op.disassemble()).collect()
    }

    pub fn disassemble_to_string(&self) -> String {
        self.disassemble().join("\n")
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        self.instructions.iter().flat_map(|op| op.to_bytes()).collect()
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<Self> {
        if bytes.is_empty() {
            return Err(Error::EmptyInstructionBytes);
        }
        let mut bytecode = Bytecode::new();
        let mut offset = 0;
        while offset < bytes.len() {
            bytecode.push(Instruction::from_bytes(bytes, &mut offset)?);
        }
        Ok(bytecode)
    }
}

impl From<Vec<Instruction>> for Bytecode {
    fn from(instructions: Vec<Instruction>) -> Self {
        Self { instructions }
    }
}

impl From<&[Instruction]> for Bytecode {
    fn from(instructions: &[Instruction]) -> Self {
        Self {
            instructions: instructions.to_vec(),
        }
    }
}

impl TryFrom<&str> for Bytecode {
    type Error = Error;

    fn try_from(assembly: &str) -> std::result::Result<Self, Self::Error> {
        Self::assemble_from_string(assembly)
    }
}

impl TryFrom<&[u8]> for Bytecode {
    type Error = Error;

    fn try_from(bytes: &[u8]) -> std::result::Result<Self, Self::Error> {
        Self::from_bytes(bytes)
    }
}

impl Display for Bytecode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let instructions: Vec<String> = self
            .instructions
            .iter()
            .map(|instruction| instruction.to_string())
            .collect();
        write!(f, "== Bytecode (Disassembled) ==\n{}", instructions.join("\n"))
    }
}
