use std::fmt::{Debug, Display, Formatter};

pub type Result<T> = std::result::Result<T, Error>;

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

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq)]
pub enum Version {
    V1,
    V2,
    V3,
}

impl Version {
    pub fn latest() -> Self {
        Self::V1
    }

    pub fn preview() -> Option<Self> {
        None
    }

    pub fn to_u8(&self) -> u8 {
        match self {
            Self::V1 => 1,
            Self::V2 => 2,
            Self::V3 => 3,
        }
    }

    pub fn from_u8(version: u8) -> Option<Self> {
        Some(match version {
            1 => Self::V1,
            2 => Self::V2,
            3 => Self::V3,
            _ => return None,
        })
    }

    pub fn is_valid(version: u8) -> bool {
        Self::from_u8(version).is_some()
    }
}

impl Default for Version {
    fn default() -> Self {
        Self::latest()
    }
}

impl From<u8> for Version {
    fn from(version: u8) -> Self {
        Self::from_u8(version).expect("Invalid version")
    }
}

impl Display for Version {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "v{}", self.to_u8())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ConstantPoolEntry {
    Double(f64),
    Float(f32),
    Long(i64),
    Int(i32),
}

impl ConstantPoolEntry {
    pub fn tag(&self) -> u8 {
        match self {
            ConstantPoolEntry::Double(_) => 0b_0100_0110, // 'F'
            ConstantPoolEntry::Float(_) => 0b_0110_0110,  // 'f'
            ConstantPoolEntry::Long(_) => 0b_0100_1001,   // 'I'
            ConstantPoolEntry::Int(_) => 0b_0110_1001,    // 'i'
        }
    }
}

impl Display for ConstantPoolEntry {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstantPoolEntry::Double(value) => write!(f, "{}", value),
            ConstantPoolEntry::Float(value) => write!(f, "{}", value),
            ConstantPoolEntry::Long(value) => write!(f, "{}", value),
            ConstantPoolEntry::Int(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LineNumberEntry {
    pub start_pc: usize,
    pub line_number: u16,
}

impl LineNumberEntry {
    pub fn new(start_pc: usize, line_number: u16) -> Self {
        Self {
            start_pc,
            line_number,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    LoadConstant8 { cp_addr: u8 },
    LoadConstant16 { cp_addr: u16 },
    Return,
    SomeInstructionThatUsesMemory { operand1: usize, operand2: usize },
}

impl Instruction {
    pub fn opcode(&self) -> u8 {
        match self {
            Instruction::LoadConstant8 { .. } => 0b_0110_0011,  // 'c'
            Instruction::LoadConstant16 { .. } => 0b_0100_0011, // 'C'
            Instruction::Return => 0b_0111_0010,                // 'r'
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Instruction::LoadConstant8 { cp_addr } => format!("const [{}]", cp_addr),
                Instruction::LoadConstant16 { cp_addr } => format!("const [{}]", cp_addr),
                Instruction::Return => "ret".to_string(),
            }
        )
    }
}

#[derive(Debug, Clone, Default)]
pub struct KlassBytecode {
    name: Option<String>,
    version: Version,
    instructions: Vec<Instruction>,
    constant_pool: Vec<ConstantPoolEntry>,
    line_number_table: Vec<LineNumberEntry>,
}

impl KlassBytecode {
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

    fn get_line_number(&self, code_addr: usize) -> Option<u16> {
        let entry_index = self
            .line_number_table
            .binary_search_by(|entry| entry.start_pc.cmp(&code_addr))
            .map(Some)
            .unwrap_or_else(|index| index.checked_sub(1))?;
        self.line_number_table
            .get(entry_index)
            .map(|entry| entry.line_number)
    }

    pub fn name(&self) -> &Option<String> {
        &self.name
    }

    pub fn version(&self) -> Version {
        self.version
    }

    pub fn instructions(&self) -> &Vec<Instruction> {
        &self.instructions
    }

    pub fn constant_pool(&self) -> &Vec<ConstantPoolEntry> {
        &self.constant_pool
    }

    pub fn line_number_table(&self) -> &Vec<LineNumberEntry> {
        &self.line_number_table
    }
}

impl Display for KlassBytecode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let instructions: Vec<String> = self
            .instructions
            .iter()
            .enumerate()
            .map(|(i, instruction)| {
                let value_at_cp_addr = match instruction {
                    Instruction::LoadConstant8 { cp_addr } => {
                        Some(self.constant_pool[*cp_addr as usize].to_string())
                    }
                    Instruction::LoadConstant16 { cp_addr } => {
                        Some(self.constant_pool[*cp_addr as usize].to_string())
                    }
                    _ => None,
                };
                let prev_line = i.checked_sub(1).and_then(|i| self.get_line_number(i));
                let line_number = self.get_line_number(i);
                let line = match (prev_line, line_number) {
                    (Some(prev), Some(line)) if prev == line => "    |".to_string(),
                    _ => {
                        if let Some(line) = line_number {
                            format!("{:>5}", line)
                        } else {
                            "    ?".to_string()
                        }
                    }
                };
                if let Some(extra) = value_at_cp_addr {
                    format!("{:04} {} {} # {}", instruction.opcode(), line, instruction, extra)
                } else {
                    format!("{:04} {} {}", instruction.opcode(), line, instruction)
                }
            })
            .collect();

        let title = self
            .name
            .as_ref()
            .map(|name| format!("{} Bytecode", name))
            .unwrap_or_else(|| "Bytecode".to_string());
        write!(f, "== {} {} ==\n{}", title, self.version, instructions.join("\n"))
    }
}
