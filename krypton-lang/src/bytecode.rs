use std::fmt::{Debug, Display, Formatter};

use crate::bytecode::Instruction::Return;

#[derive(Debug, Clone)]
pub enum Error {
    InvalidBytesSize { expected: usize, actual: usize },
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Error::InvalidBytesSize { expected, actual } => {
                    format!("Invalid bytes size: expected {}, got {}", expected, actual)
                }
            }
        )
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, Copy)]
pub struct Version {
    major: u8,
    minor: u8,
}

impl Version {
    pub fn new(major: u8, minor: u8) -> Self {
        Self { major, minor }
    }

    pub fn current() -> Self {
        // NOTE make sure to update this when the version changes
        Self::new(0, 1)
    }

    pub fn major(&self) -> u8 {
        self.major
    }

    pub fn minor(&self) -> u8 {
        self.minor
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        vec![self.major, self.minor]
    }

    pub fn to_u16(&self) -> u16 {
        (self.major as u16) << 8 | (self.minor as u16)
    }
}

impl From<u16> for Version {
    fn from(version: u16) -> Self {
        Self {
            major: (version >> 8) as u8,
            minor: (version & 0xFF) as u8,
        }
    }
}

impl TryFrom<&[u8]> for Version {
    type Error = Error;

    fn try_from(version: &[u8]) -> Result<Self> {
        if version.len() != 2 {
            return Err(Error::InvalidBytesSize {
                expected: 2,
                actual: version.len(),
            });
        }
        Ok(Self::from(u16::from_be_bytes([version[0], version[1]])))
    }
}

impl Default for Version {
    fn default() -> Self {
        Self::current()
    }
}

impl Display for Version {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.major, self.minor)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ConstantPoolEntry {
    Double(f64),
    Int(i32),
}

impl Display for ConstantPoolEntry {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstantPoolEntry::Double(value) => write!(f, "{}", value),
            ConstantPoolEntry::Int(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LineNumberEntry {
    start_pc: u16,
    length: u16,
    line_number: u16,
    column_number: u16,
}

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    Constant { cp_addr: u16 },
    Return,
}

impl Instruction {
    pub fn opcode(&self) -> u8 {
        match self {
            Instruction::Constant { .. } => 0b_0000_0000,
            Return => 0b_0000_0001,
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:04} {}",
            self.opcode(),
            match self {
                Instruction::Constant { cp_addr } => {
                    format!("const [{}]", cp_addr)
                }
                Return => "ret".to_string(),
            }
        )
    }
}

#[derive(Debug, Clone, Default)]
pub struct Bytecode {
    name: Option<String>,
    version: Version,
    instructions: Vec<Instruction>,
    constant_pool: Vec<ConstantPoolEntry>,
    line_number_table: Vec<LineNumberEntry>,
}

impl Bytecode {
    pub fn new(name: String, version: Version) -> Self {
        Self {
            name: Some(name),
            version,
            ..Default::default()
        }
    }

    pub fn with_name(name: String) -> Self {
        Self {
            name: Some(name),
            ..Default::default()
        }
    }

    pub fn with_version(version: Version) -> Self {
        Self {
            version,
            ..Default::default()
        }
    }

    pub fn push_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub fn push_constant(&mut self, constant: ConstantPoolEntry) {
        self.constant_pool.push(constant);
    }

    pub fn push_line_number(&mut self, line_number: LineNumberEntry) {
        self.line_number_table.push(line_number);
    }

    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    pub fn is_empty(&self) -> bool {
        self.instructions.is_empty()
    }

    pub fn version(&self) -> Version {
        self.version
    }
}

impl Display for Bytecode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let instructions: Vec<String> = self
            .instructions
            .iter()
            .map(|instruction| {
                let value_at_cp_addr = match instruction {
                    Instruction::Constant { cp_addr } => {
                        Some(self.constant_pool[*cp_addr as usize].to_string())
                    }
                    _ => None,
                };
                if let Some(extra) = value_at_cp_addr {
                    format!("{} # {}", instruction, extra)
                } else {
                    format!("{}", instruction)
                }
            })
            .collect();
        let title = self
            .name
            .as_ref()
            .map(|name| format!("{} Bytecode", name))
            .unwrap_or_else(|| "Bytecode".to_string());
        write!(
            f,
            "== {} v{} ==\n{}",
            title,
            self.version,
            instructions.join("\n")
        )
    }
}
