use crate::compiler::lexer::{is_identifier_continue, is_identifier_start};
use std::fmt::{Debug, Display, Formatter};
use std::mem::size_of;

pub type Result<T> = std::result::Result<T, Error>;
pub type Version = u8;

#[derive(Debug)]
pub enum Error {
    InvalidMagicNumber { expected: u8, got: u8 },
    IdentifierTooLong(usize),
    IdentifierContainsNonAsciiChar(char),
    IdentifierContainsIllegalChar(char),
    IdentifierCannotBeEmpty,
    BytecodeOutOfBoundsExpectedVersion,
    BytecodeOutOfBoundsExpectedMagicNumber,
    BytecodeOutOfBoundsExpectedNameLength,
    BytecodeOutOfBoundsExpectedName(usize),
    BytecodeOutOfBoundsExpectedConstantPoolSize,
    BytecodeSizeTooSmall { expected: usize, got: usize },
    InvalidTag(u8),
    BytecodeOutOfBoundsExpectedConstantPoolEntry,
    BytecodeOutOfBoundsExpectedInstructionsSize,
    BytecodeOutOfBoundsExpectedOpcode,
    UnrecognizedOpcode(u8),
    BytecodeOutOfBoundsExpectedOperands(usize),
    BytecodeOutOfBoundsExpectedConstantPoolValue(usize),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Error::InvalidMagicNumber { expected, got } => format!(
                "Invalid magic number. Expected {} '{}', got {} '{}'",
                expected, *expected as char, got, *got as char,
            ),
            Error::IdentifierTooLong(length) => format!(
                "Identifier too long. Expected at most {} bytes, got {}",
                255, length
            ),
            Error::IdentifierContainsNonAsciiChar(c) =>
                format!("Identifier contains non-ASCII character '{}'", c),
            Error::IdentifierContainsIllegalChar(c) => format!("Identifier contains illegal character '{}'", c),
            Error::IdentifierCannotBeEmpty => "Identifier cannot be empty".to_string(),
            Error::BytecodeOutOfBoundsExpectedMagicNumber =>
                "Bytecode out of bounds. Expected at least 2 magic number bytes, got 0".to_string(),
            Error::BytecodeOutOfBoundsExpectedVersion =>
                "Bytecode out of bounds. Expected at least 2 version bytes, got 0".to_string(),
            Error::BytecodeOutOfBoundsExpectedNameLength =>
                "Bytecode out of bounds. Expected at least 1 name length byte, got 0".to_string(),
            Error::BytecodeOutOfBoundsExpectedName(name_length) =>
                format!("Bytecode out of bounds. Expected {} bytes", name_length),
            Error::BytecodeOutOfBoundsExpectedConstantPoolSize =>
                "Bytecode out of bounds. Expected at least 1 byte, got 0".to_string(),
            Error::BytecodeSizeTooSmall { expected, got } => format!(
                "Bytecode size too small. Expected at least {} bytes, got {}",
                expected, got
            ),
            Error::InvalidTag(tag) => format!("Invalid tag: {}", tag),
            Error::BytecodeOutOfBoundsExpectedConstantPoolEntry =>
                "Bytecode out of bounds. Expected at least 1 constant value byte".to_string(),
            Error::BytecodeOutOfBoundsExpectedInstructionsSize =>
                "Bytecode out of bounds. Expected 8 bytes for instructions size".to_string(),
            Error::BytecodeOutOfBoundsExpectedOpcode =>
                "Bytecode out of bounds. Expected at least 1 opcode byte, got 0".to_string(),
            Error::UnrecognizedOpcode(opcode) => format!("Unrecognized opcode: {}", opcode),
            Error::BytecodeOutOfBoundsExpectedOperands(operands) =>
                format!("Bytecode out of bounds. Expected {} operand bytes", operands),
            Error::BytecodeOutOfBoundsExpectedConstantPoolValue(value) =>
                format!("Bytecode out of bounds. Expected {} constant value bytes", value),
        })
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum Tag {
    Double = 0b_0100_0110, // 'F'
    Float = 0b_0110_0110,  // 'f'
    Long = 0b_0100_1001,   // 'I'
    Int = 0b_0110_1001,    // 'i'
}

impl Tag {
    pub fn from_u8(tag: u8) -> Result<Self> {
        match tag {
            0b_0100_0110 => Ok(Tag::Double),
            0b_0110_0110 => Ok(Tag::Float),
            0b_0100_1001 => Ok(Tag::Long),
            0b_0110_1001 => Ok(Tag::Int),
            _ => Err(Error::InvalidTag(tag)),
        }
    }

    pub fn entry_size(&self) -> usize {
        match self {
            Tag::Double => 8,
            Tag::Float => 4,
            Tag::Long => 8,
            Tag::Int => 4,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Double(f64),
    Float(f32),
    Long(i64),
    Int(i32),
}

impl Value {
    pub fn tag(&self) -> Tag {
        match self {
            Value::Double(_) => Tag::Double,
            Value::Float(_) => Tag::Float,
            Value::Long(_) => Tag::Long,
            Value::Int(_) => Tag::Int,
        }
    }

    pub fn to_be_bytes(&self) -> Vec<u8> {
        match self {
            Value::Double(value) => value.to_be_bytes().to_vec(),
            Value::Float(value) => value.to_be_bytes().to_vec(),
            Value::Long(value) => value.to_be_bytes().to_vec(),
            Value::Int(value) => value.to_be_bytes().to_vec(),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Value::Double(value) => value.to_string(),
            Value::Float(value) => value.to_string(),
            Value::Long(value) => value.to_string(),
            Value::Int(value) => value.to_string(),
        })
    }
}

impl std::ops::Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Value::Double(value) => Value::Double(-value),
            Value::Float(value) => Value::Float(-value),
            Value::Long(value) => Value::Long(-value),
            Value::Int(value) => Value::Int(-value),
        }
    }
}

impl std::ops::Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Double(lhs), Value::Double(rhs)) => Value::Double(lhs + rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
            (Value::Long(lhs), Value::Long(rhs)) => Value::Long(lhs + rhs),
            (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs + rhs),
            _ => panic!("Invalid operands"),
        }
    }
}

impl std::ops::Sub for Value {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Double(lhs), Value::Double(rhs)) => Value::Double(lhs - rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs - rhs),
            (Value::Long(lhs), Value::Long(rhs)) => Value::Long(lhs - rhs),
            (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs - rhs),
            _ => panic!("Invalid operands"),
        }
    }
}

impl std::ops::Mul for Value {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Double(lhs), Value::Double(rhs)) => Value::Double(lhs * rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs * rhs),
            (Value::Long(lhs), Value::Long(rhs)) => Value::Long(lhs * rhs),
            (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs * rhs),
            _ => panic!("Invalid operands"),
        }
    }
}

impl std::ops::Div for Value {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Double(lhs), Value::Double(rhs)) => Value::Double(lhs / rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs / rhs),
            (Value::Long(lhs), Value::Long(rhs)) => Value::Long(lhs / rhs),
            (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs / rhs),
            _ => panic!("Invalid operands"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LineNumberEntry {
    pub start_pc: usize,
    pub line_number: u16,
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum Opcode {
    LoadConstant = 0b_01000011, // 'C'
    Return = 0b_01110010,       // 'r'
    Negate = 0b_00100001,       // '!'
    Add = 0b_00101011,          // '+'
    Subtract = 0b_00101101,     // '-'
    Multiply = 0b_00101010,     // '*'
    Divide = 0b_00101111,       // '/'
}

impl Opcode {
    pub fn from_u8(opcode: u8) -> Result<Self> {
        Ok(match opcode {
            0b_01000011 => Opcode::LoadConstant, // 'C'
            0b_01110010 => Opcode::Return,       // 'r'
            0b_00100001 => Opcode::Negate,       // '!'
            0b_00101011 => Opcode::Add,          // '+'
            0b_00101101 => Opcode::Subtract,     // '-'
            0b_00101010 => Opcode::Multiply,     // '*'
            0b_00101111 => Opcode::Divide,       // '/'
            _ => return Err(Error::UnrecognizedOpcode(opcode)),
        })
    }

    pub fn operands_size(&self) -> usize {
        match self {
            Opcode::LoadConstant => 2,
            Opcode::Return => 0,
            Opcode::Negate => 0,
            Opcode::Add => 0,
            Opcode::Subtract => 0,
            Opcode::Multiply => 0,
            Opcode::Divide => 0,
        }
    }
}

impl Display for Opcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Opcode::LoadConstant => "const",
            Opcode::Return => "ret",
            Opcode::Negate => "neg",
            Opcode::Add => "add",
            Opcode::Subtract => "sub",
            Opcode::Multiply => "mul",
            Opcode::Divide => "div",
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    LoadConstant { cp_addr: u16 },
    Return,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl Instruction {
    pub fn opcode(&self) -> Opcode {
        match self {
            Instruction::LoadConstant { .. } => Opcode::LoadConstant,
            Instruction::Return => Opcode::Return,
            Instruction::Negate => Opcode::Negate,
            Instruction::Add => Opcode::Add,
            Instruction::Subtract => Opcode::Subtract,
            Instruction::Multiply => Opcode::Multiply,
            Instruction::Divide => Opcode::Divide,
        }
    }

    pub fn to_be_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.push(self.opcode() as u8);
        match self {
            Instruction::LoadConstant { cp_addr } => bytes.extend(cp_addr.to_be_bytes()),
            Instruction::Return => {}
            Instruction::Negate => {}
            Instruction::Add => {}
            Instruction::Subtract => {}
            Instruction::Multiply => {}
            Instruction::Divide => {}
        }
        bytes
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Instruction::LoadConstant { cp_addr } => format!("const [{}]", cp_addr),
            Instruction::Return => "ret".to_string(),
            Instruction::Negate => "neg".to_string(),
            Instruction::Add => "add".to_string(),
            Instruction::Subtract => "sub".to_string(),
            Instruction::Multiply => "mul".to_string(),
            Instruction::Divide => "div".to_string(),
        })
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    name: String,
}

impl Identifier {
    pub fn new(name: &str) -> Result<Self> {
        if name.len() > 255 {
            return Err(Error::IdentifierTooLong(name.len()));
        }
        if name.is_empty() {
            return Err(Error::IdentifierCannotBeEmpty);
        }

        // First char
        if !is_identifier_start(name.chars().next().expect("More than 1 character")) {
            return Err(Error::IdentifierCannotBeEmpty);
        }

        for c in name.chars() {
            if !c.is_ascii() {
                return Err(Error::IdentifierContainsNonAsciiChar(c));
            }
            if !is_identifier_continue(c) {
                return Err(Error::IdentifierContainsIllegalChar(c));
            }
        }
        Ok(Self {
            name: name.to_string(),
        })
    }

    pub fn new_unchecked(name: String) -> Self {
        Self { name }
    }

    pub fn as_str(&self) -> &str {
        &self.name
    }

    pub fn len(&self) -> u8 {
        assert!((1..=255).contains(&self.name.len()));
        self.name.len() as u8
    }

    pub fn is_empty(&self) -> bool {
        self.name.is_empty()
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.name.as_bytes()
    }
}

impl TryFrom<&str> for Identifier {
    type Error = Error;
    fn try_from(value: &str) -> Result<Self> {
        Self::new(value)
    }
}

impl TryFrom<String> for Identifier {
    type Error = Error;
    fn try_from(value: String) -> Result<Self> {
        Self::new(&value)
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone, Default)]
pub struct Klass {
    pub name: Option<Identifier>,
    pub version: Version,
    pub constant_pool: Vec<Value>,
    pub instructions: Vec<Instruction>,
    pub line_number_table: Option<Vec<LineNumberEntry>>,
}

const MAGIC_NUMBER: [u8; 3] = [b'K', b'r', b'V'];
const VERSION_OFFSET: usize = 2;
const NAME_LENGTH_OFFSET: usize = 4;

#[allow(clippy::vec_init_then_push)]
pub fn encode(klass: &Klass) -> Vec<u8> {
    let mut bytecode = Vec::new();

    // Metadata
    bytecode.push(MAGIC_NUMBER[0]);
    bytecode.push(MAGIC_NUMBER[1]);
    bytecode.push(MAGIC_NUMBER[2]);
    bytecode.push(klass.version);

    if let Some(name) = &klass.name {
        bytecode.push(name.len());
        bytecode.extend(name.as_bytes());
    } else {
        bytecode.push(0);
    }

    // Constant pool
    let mut constant_pool_size: u16 = 0;
    let mut constant_pool = Vec::new();
    for constant_pool_entry in &klass.constant_pool {
        constant_pool.push(constant_pool_entry.tag() as u8);
        let value_bytes = constant_pool_entry.to_be_bytes();
        constant_pool_size += 1 + value_bytes.len() as u16;
        constant_pool.extend(value_bytes);
    }
    bytecode.extend(constant_pool_size.to_be_bytes());
    bytecode.extend(constant_pool);

    // Instructions
    let mut instructions_size: u64 = 0;
    let mut instructions = Vec::new();
    for instruction in &klass.instructions {
        let instruction_bytes = instruction.to_be_bytes();
        instructions_size += instruction_bytes.len() as u64;
        instructions.extend(instruction_bytes);
    }
    bytecode.extend(instructions_size.to_be_bytes());
    bytecode.extend(instructions);

    bytecode
}

pub fn decode(bytecode: &[u8]) -> Result<Klass> {
    let mut klass = Klass::default();

    // Metadata
    #[allow(clippy::needless_range_loop)]
    for i in 0..3 {
        let magic_number = *bytecode
            .get(i)
            .ok_or(Error::BytecodeOutOfBoundsExpectedMagicNumber)?;
        if magic_number != MAGIC_NUMBER[i] {
            return Err(Error::InvalidMagicNumber {
                expected: MAGIC_NUMBER[i],
                got: magic_number,
            });
        }
    }

    klass.version = *bytecode
        .get(VERSION_OFFSET)
        .ok_or(Error::BytecodeOutOfBoundsExpectedVersion)?;

    let name_length = *bytecode
        .get(NAME_LENGTH_OFFSET)
        .ok_or(Error::BytecodeOutOfBoundsExpectedNameLength)?;

    klass.name = if name_length > 0 {
        let name_bytes = bytecode
            .get(NAME_LENGTH_OFFSET + 1..NAME_LENGTH_OFFSET + 1 + name_length as usize)
            .ok_or(Error::BytecodeOutOfBoundsExpectedName(name_length as usize))?;
        Some(Identifier::new(
            &String::from_utf8(name_bytes.to_vec()).expect("Invalid UTF-8"),
        )?)
    } else {
        None
    };

    let mut offset = NAME_LENGTH_OFFSET + 1 + name_length as usize;
    let constant_pool_size = bytecode
        .get(offset..offset + size_of::<u16>())
        .ok_or(Error::BytecodeOutOfBoundsExpectedConstantPoolSize)?;
    if constant_pool_size.len() != size_of::<u16>() {
        return Err(Error::BytecodeOutOfBoundsExpectedConstantPoolSize);
    }
    let constant_pool_size = u16::from_be_bytes(constant_pool_size.try_into().unwrap());

    offset += size_of::<u16>();
    if (bytecode.len() - offset) < constant_pool_size as usize {
        return Err(Error::BytecodeSizeTooSmall {
            expected: constant_pool_size as usize,
            got: bytecode.len() - offset,
        });
    }

    let cp_start = offset;
    while offset < cp_start + constant_pool_size as usize {
        let tag: Tag = Tag::from_u8(
            *bytecode
                .get(offset)
                .ok_or(Error::BytecodeOutOfBoundsExpectedConstantPoolEntry)?,
        )?;
        let entry_size = tag.entry_size();
        let entry_bytes = bytecode
            .get(offset + 1..offset + 1 + entry_size)
            .ok_or(Error::BytecodeOutOfBoundsExpectedConstantPoolValue(entry_size))?;
        let entry = match tag {
            Tag::Double => Value::Double(f64::from_be_bytes(entry_bytes.try_into().unwrap())),
            Tag::Float => Value::Float(f32::from_be_bytes(entry_bytes.try_into().unwrap())),
            Tag::Long => Value::Long(i64::from_be_bytes(entry_bytes.try_into().unwrap())),
            Tag::Int => Value::Int(i32::from_be_bytes(entry_bytes.try_into().unwrap())),
        };
        klass.constant_pool.push(entry);
        offset += 1 + entry_size;
    }

    let instructions_size = bytecode
        .get(offset..offset + size_of::<u64>())
        .ok_or(Error::BytecodeOutOfBoundsExpectedInstructionsSize)?;
    if instructions_size.len() != size_of::<u64>() {
        return Err(Error::BytecodeOutOfBoundsExpectedInstructionsSize);
    }
    let instructions_size = u64::from_be_bytes(instructions_size.try_into().unwrap());

    offset += size_of::<u64>();
    if (bytecode.len() - offset) < instructions_size as usize {
        return Err(Error::BytecodeSizeTooSmall {
            expected: instructions_size as usize,
            got: bytecode.len() - offset,
        });
    }

    let instructions_start = offset;
    debug!("Offset (init): {}", instructions_start);
    while offset < instructions_start + instructions_size as usize {
        debug!("Offset (before): {}", offset);
        let opcode: Opcode = Opcode::from_u8(
            *bytecode
                .get(offset)
                .ok_or(Error::BytecodeOutOfBoundsExpectedOpcode)?,
        )?;
        debug!("Opcode: {}", opcode);
        let operands_size = opcode.operands_size();
        let instruction_bytes = if operands_size == 0 {
            None
        } else {
            Some(
                bytecode
                    .get((offset + 1)..(offset + 1 + operands_size))
                    .ok_or(Error::BytecodeOutOfBoundsExpectedOperands(operands_size))?,
            )
        };
        let instruction = match opcode {
            Opcode::LoadConstant => Instruction::LoadConstant {
                cp_addr: u16::from_be_bytes(instruction_bytes.unwrap().try_into().unwrap()),
            },
            Opcode::Return => Instruction::Return,
            Opcode::Negate => Instruction::Negate,
            Opcode::Add => Instruction::Add,
            Opcode::Subtract => Instruction::Subtract,
            Opcode::Multiply => Instruction::Multiply,
            Opcode::Divide => Instruction::Divide,
        };
        klass.instructions.push(instruction);
        offset += 1 + operands_size;
        debug!("Offset (after): {}", offset);
    }

    Ok(klass)
}

pub trait BinaryDisplayExt {
    fn display_binary(&self) -> String;
}

impl BinaryDisplayExt for &[u8] {
    fn display_binary(&self) -> String {
        format!(
            "|  Binary  | Hex | Dec | Char |\n{}\n{}\n",
            "-".repeat(31),
            self.iter()
                .map(|byte| byte.display_binary())
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

impl BinaryDisplayExt for u8 {
    fn display_binary(&self) -> String {
        format!(
            "| {:08b} | {:02x}  | {:03} | '{}'  |",
            self,
            self,
            self,
            match self {
                b' ' => ' ',
                _ if (32..=126).contains(self) => *self as char,
                _ => '�',
            }
        )
    }
}

impl BinaryDisplayExt for Vec<u8> {
    fn display_binary(&self) -> String {
        self.as_slice().display_binary()
    }
}
