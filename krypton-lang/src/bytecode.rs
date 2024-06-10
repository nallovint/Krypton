use std::fmt::{Debug, Display, Formatter};
use std::mem::size_of;

use crate::compiler::lex::{is_identifier_continue, is_identifier_start};

pub type Result<T> = std::result::Result<T, Error>;
pub type InstructionsLength = u32;
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
            Self::InvalidMagicNumber { expected, got } => format!(
                "Invalid magic number. Expected {} '{}', got {} '{}'",
                expected, *expected as char, got, *got as char,
            ),
            Self::IdentifierTooLong(length) =>
                format!("Identifier too long. Expected at most {} bytes, got {}", 255, length),
            Self::IdentifierContainsNonAsciiChar(c) => format!("Identifier contains non-ASCII character '{c}'"),
            Self::IdentifierContainsIllegalChar(c) => format!("Identifier contains illegal character '{c}'"),
            Self::IdentifierCannotBeEmpty => "Identifier cannot be empty".to_owned(),
            Error::BytecodeOutOfBoundsExpectedMagicNumber =>
                "Bytecode out of bounds. Expected at least 2 magic number bytes, got 0".to_owned(),
            Error::BytecodeOutOfBoundsExpectedVersion =>
                "Bytecode out of bounds. Expected at least 2 version bytes, got 0".to_owned(),
            Error::BytecodeOutOfBoundsExpectedNameLength =>
                "Bytecode out of bounds. Expected at least 1 name length byte, got 0".to_owned(),
            Error::BytecodeOutOfBoundsExpectedName(name_length) =>
                format!("Bytecode out of bounds. Expected {name_length} bytes"),
            Error::BytecodeOutOfBoundsExpectedConstantPoolSize =>
                "Bytecode out of bounds. Expected at least 1 byte, got 0".to_owned(),
            Error::BytecodeSizeTooSmall { expected, got } =>
                format!("Bytecode size too small. Expected at least {expected} bytes, got {got}",),
            Error::InvalidTag(tag) => format!("Invalid tag: {tag}"),
            Error::BytecodeOutOfBoundsExpectedConstantPoolEntry =>
                "Bytecode out of bounds. Expected at least 1 constant value byte".to_owned(),
            Error::BytecodeOutOfBoundsExpectedInstructionsSize =>
                "Bytecode out of bounds. Expected 8 bytes for instructions size".to_owned(),
            Error::BytecodeOutOfBoundsExpectedOpcode =>
                "Bytecode out of bounds. Expected at least 1 opcode byte, got 0".to_owned(),
            Error::UnrecognizedOpcode(opcode) => format!("Unrecognized opcode: {opcode}"),
            Error::BytecodeOutOfBoundsExpectedOperands(operands) =>
                format!("Bytecode out of bounds. Expected {operands} operand bytes"),
            Error::BytecodeOutOfBoundsExpectedConstantPoolValue(value) =>
                format!("Bytecode out of bounds. Expected {value} constant value bytes"),
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
    #[allow(clippy::missing_errors_doc)]
    pub const fn from_u8(tag: u8) -> Result<Self> {
        match tag {
            0b_0100_0110 => Ok(Tag::Double),
            0b_0110_0110 => Ok(Tag::Float),
            0b_0100_1001 => Ok(Tag::Long),
            0b_0110_1001 => Ok(Tag::Int),
            _ => Err(Error::InvalidTag(tag)),
        }
    }

    #[must_use]
    pub const fn entry_size(&self) -> usize {
        match self {
            Tag::Double | Tag::Long => 8,
            Tag::Float | Tag::Int => 4,
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
    #[must_use]
    pub const fn tag(&self) -> Tag {
        match self {
            Value::Double(_) => Tag::Double,
            Value::Float(_) => Tag::Float,
            Value::Long(_) => Tag::Long,
            Value::Int(_) => Tag::Int,
        }
    }

    #[must_use]
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
    #[allow(clippy::missing_errors_doc)]
    pub const fn from_u8(opcode: u8) -> Result<Self> {
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

    #[must_use]
    pub const fn operands_size(&self) -> usize {
        match self {
            Opcode::LoadConstant => 2,
            Opcode::Return
            | Opcode::Negate
            | Opcode::Add
            | Opcode::Subtract
            | Opcode::Multiply
            | Opcode::Divide => 0,
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
    #[must_use]
    pub const fn opcode(&self) -> Opcode {
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

    #[must_use]
    pub fn to_be_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.push(self.opcode() as u8);
        match self {
            Instruction::LoadConstant { cp_addr } => bytes.extend(cp_addr.to_be_bytes()),
            Instruction::Return
            | Instruction::Negate
            | Instruction::Add
            | Instruction::Subtract
            | Instruction::Multiply
            | Instruction::Divide => {}
        }
        bytes
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Instruction::LoadConstant { cp_addr } => format!("const [{cp_addr}]"),
            Instruction::Return => "ret".to_owned(),
            Instruction::Negate => "neg".to_owned(),
            Instruction::Add => "add".to_owned(),
            Instruction::Subtract => "sub".to_owned(),
            Instruction::Multiply => "mul".to_owned(),
            Instruction::Divide => "div".to_owned(),
        })
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    name: String,
}

impl Identifier {
    #[allow(clippy::missing_errors_doc)]
    #[allow(clippy::missing_panics_doc)]
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
            name: name.to_owned(),
        })
    }

    #[must_use]
    #[allow(clippy::missing_safety_doc)]
    pub const unsafe fn new_unchecked(name: String) -> Self {
        Self { name }
    }

    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.name
    }

    #[must_use]
    #[allow(clippy::missing_panics_doc)]
    pub fn len(&self) -> u8 {
        u8::try_from(self.name.len()).expect("Invalid Identifier, must be between 1 and 255 bytes")
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.name.is_empty()
    }

    #[must_use]
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
    pub line_number_table: Vec<LineNumberEntry>,
}

const MAGIC_NUMBER: [u8; 3] = [b'K', b'r', b'V'];
const VERSION_OFFSET: usize = 2;
const NAME_LENGTH_OFFSET: usize = 4;

#[must_use]
#[allow(clippy::vec_init_then_push)]
#[allow(clippy::missing_panics_doc)]
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
        constant_pool_size += 1 + u16::try_from(value_bytes.len()).expect("Invalid constant pool entry");
        constant_pool.extend(value_bytes);
    }
    bytecode.extend(constant_pool_size.to_be_bytes());
    bytecode.extend(constant_pool);

    // Instructions
    let mut instructions_size: InstructionsLength = 0;
    let mut instructions = Vec::new();
    for instruction in &klass.instructions {
        let instruction_bytes = instruction.to_be_bytes();
        instructions_size += InstructionsLength::try_from(instruction_bytes.len()).expect("Invalid length");
        instructions.extend(instruction_bytes);
    }
    bytecode.extend(instructions_size.to_be_bytes());
    bytecode.extend(instructions);

    bytecode
}

macro_rules! cmp_magic_byte {
    ($bytecode:expr, $i:expr) => {
        let magic_number = *$bytecode
            .get($i)
            .ok_or(Error::BytecodeOutOfBoundsExpectedMagicNumber)?;
        if magic_number != MAGIC_NUMBER[$i] {
            return Err(Error::InvalidMagicNumber {
                expected: MAGIC_NUMBER[$i],
                got: magic_number,
            });
        }
    };
}

#[allow(clippy::missing_errors_doc)]
#[allow(clippy::missing_panics_doc)]
pub fn decode(bytecode: &[u8]) -> Result<Klass> {
    let mut klass = Klass::default();

    // Magic number
    cmp_magic_byte!(bytecode, 0);
    cmp_magic_byte!(bytecode, 1);

    // Version
    cmp_magic_byte!(bytecode, 2);
    klass.version = *bytecode
        .get(VERSION_OFFSET)
        .ok_or(Error::BytecodeOutOfBoundsExpectedVersion)?;

    // Name
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
    #[allow(clippy::unwrap_used)]
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
        #[allow(clippy::unwrap_used)]
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
        .get(offset..offset + size_of::<InstructionsLength>())
        .ok_or(Error::BytecodeOutOfBoundsExpectedInstructionsSize)?;
    if instructions_size.len() != size_of::<InstructionsLength>() {
        return Err(Error::BytecodeOutOfBoundsExpectedInstructionsSize);
    }
    #[allow(clippy::unwrap_used)]
    let instructions_size = InstructionsLength::from_be_bytes(instructions_size.try_into().unwrap());

    offset += size_of::<InstructionsLength>();
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
        #[allow(clippy::unwrap_used)]
        let instruction = match opcode {
            Opcode::LoadConstant => Instruction::LoadConstant {
                cp_addr: u16::from_be_bytes(
                    instruction_bytes
                        .expect("2-bytes long operand")
                        .try_into()
                        .unwrap(),
                ),
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
                .map(BinaryDisplayExt::display_binary)
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

impl BinaryDisplayExt for u8 {
    fn display_binary(&self) -> String {
        format!("| {:08b} | {:02x}  | {:03} | '{}'  |", self, self, self, match self {
            b' ' => ' ',
            _ if (32..=126).contains(self) => *self as char,
            _ => '�',
        })
    }
}

impl BinaryDisplayExt for Vec<u8> {
    fn display_binary(&self) -> String {
        self.as_slice().display_binary()
    }
}

#[must_use]
pub fn dump(klass: &Klass) -> String {
    /*
    Example:
    ============================
    || Core dumped V1         ||
    || (optional name)        ||
    ============================
    | = Constant pool (size) = |
    ----------------------------
    | 0 | 8 | 16
    | 1 | 4 | "Hello, world!"
    | 2 | 8 | 42.5
    ----------------------------
    | = Instructions (size)  = |
    ----------------------------
    | 0 | C | const [2]
    | 1 | r | ret
    ----------------------------
    | =  Line number table   = |
    ----------------------------
    | 0 | 500 | 1000
    | 1 | 750 | 1500
    ----------------------------
     */
    let border_length = "============================".len();
    let name = klass.name.clone().map_or(String::new(), |name| {
        format!("\n|| {:<border_length$} ||", name.as_str(), border_length = border_length)
    });

    let mut constant_pool = String::new();
    for (i, constant_pool_entry) in klass.constant_pool.iter().enumerate() {
        constant_pool
            .push_str(format!("\n| {i} | {:?} | {}", constant_pool_entry.tag(), constant_pool_entry).as_str());
    }

    let mut instructions = String::new();
    for (i, instruction) in klass.instructions.iter().enumerate() {
        instructions
            .push_str(format!("\n| {i:>5} | '{}' | {instruction}", (instruction.opcode() as u8) as char).as_str());
    }

    let mut line_number_table = String::new();
    for (i, window) in klass.line_number_table.windows(2).enumerate() {
        let line_number_entry = window[0];
        let next = window[1];
        line_number_table.push_str(
            format!(
                "\n| {i} | {} -> {}-{}",
                line_number_entry.line_number,
                line_number_entry.start_pc,
                next.start_pc - 1
            )
            .as_str(),
        );
    }
    if let Some(last) = klass.line_number_table.last() {
        line_number_table.push_str(
            format!(
                "\n| {} | {} -> {}-{}",
                klass.line_number_table.len() - 1,
                last.line_number,
                last.start_pc,
                klass.instructions.len() - 1
            )
            .as_str(),
        );
    }

    format!(
        r#"============================
|| Core dumped v{}         ||{}
============================
| = Constant pool (size) = |
----------------------------{}
----------------------------
| = Instructions (size)  = |
----------------------------{}
----------------------------
| =  Line number table   = |
----------------------------{}
----------------------------"#,
        klass.version, name, constant_pool, instructions, line_number_table
    )
}
