use std::fmt::{Display, Formatter};

use crate::bytecode::{ConstantPoolEntry, Instruction, KlassBytecode, LineNumberEntry, Version};

const SECTION_SEPARATOR: u8 = b'-';

#[derive(Debug, Clone)]
pub enum Error {
    InvalidMagicNumber(u16),
    InvalidVersion,
    InvalidNameLength(usize),
    InvalidNameCharacter(char),
    InvalidTag,
    InvalidInstruction,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Error::InvalidMagicNumber(magic_number) => {
                    format!("Invalid magic number: {}, expected Kr", magic_number)
                }
                Error::InvalidVersion => "Zero is an invalid version".to_string(),
                Error::InvalidNameLength(length) => {
                    format!("Invalid name length: {}, expected 0-255", length)
                }
                Error::InvalidNameCharacter(character) => {
                    format!(
                        "Invalid name character: {}, expected a-z, A-Z, 0-9, or _",
                        character
                    )
                }
                Error::InvalidTag => {
                    format!(
                        "'{}' is an invalid tag (reserved for indicating the start of the code section)",
                        SECTION_SEPARATOR
                    )
                }

                Error::InvalidInstruction => {
                    format!(
                        "'{}' is an invalid tag (reserved for indicating the start of the line number table section)",
                        SECTION_SEPARATOR
                    )
                }
            }
        )
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn klass_to_bytes(klass: &KlassBytecode) -> Result<Vec<u8>> {
    let mut bytes: Vec<u8> = Vec::new();

    push_magic_number(&mut bytes);
    push_version(&mut bytes, klass.version())?;
    push_name(&mut bytes, klass.name())?;
    push_constant_pool(&mut bytes, &klass.constant_pool())?;
    bytes.push(SECTION_SEPARATOR);
    push_code(&mut bytes, klass.instructions())?;
    bytes.push(SECTION_SEPARATOR);
    push_line_number_table(&mut bytes, klass.line_number_table())?;

    Ok(bytes)
}

fn push_magic_number(bytes: &mut Vec<u8>) {
    bytes.push(b'K');
    bytes.push(b'r');
}

fn push_version(bytes: &mut Vec<u8>, version: Version) -> Result<()> {
    bytes.push(b'V');
    let version = version.to_u8();
    if version == 0 {
        return Err(Error::InvalidVersion);
    }
    bytes.push(version);
    Ok(())
}

fn push_name(bytes: &mut Vec<u8>, name: &Option<String>) -> Result<()> {
    if let Some(name) = name {
        let Ok(len) = name.len().try_into() else {
            return Err(Error::InvalidNameLength(name.len()));
        };
        bytes.push(len);
        for c in name.chars() {
            let Ok(c) = u8::try_from(c) else {
                return Err(Error::InvalidNameCharacter(c));
            };
            if !(c.is_ascii_alphanumeric() || c == b'_') {
                return Err(Error::InvalidNameCharacter(c as char));
            }
            bytes.push(c);
        }
    } else {
        bytes.push(0);
    }

    Ok(())
}

fn push_constant_pool(bytes: &mut Vec<u8>, constant_pool: &&Vec<ConstantPoolEntry>) -> Result<()> {
    for entry in constant_pool.iter() {
        push_constant(bytes, entry)?;
    }
    Ok(())
}

fn push_constant(bytes: &mut Vec<u8>, constant: &ConstantPoolEntry) -> Result<()> {
    let tag = constant.tag();
    if tag == SECTION_SEPARATOR {
        return Err(Error::InvalidTag);
    }
    bytes.push(tag);
    match constant {
        ConstantPoolEntry::Double(value) => bytes.extend_from_slice(&value.to_be_bytes()),
        ConstantPoolEntry::Float(value) => bytes.extend_from_slice(&value.to_be_bytes()),
        ConstantPoolEntry::Long(value) => bytes.extend_from_slice(&value.to_be_bytes()),
        ConstantPoolEntry::Int(value) => bytes.extend_from_slice(&value.to_be_bytes()),
    }
    Ok(())
}

fn push_code(bytes: &mut Vec<u8>, code: &[Instruction]) -> Result<()> {
    for instruction in code.iter() {
        push_instruction(bytes, instruction)?;
    }
    Ok(())
}

fn push_instruction(bytes: &mut Vec<u8>, instruction: &Instruction) -> Result<()> {
    let opcode = instruction.opcode();
    if opcode == SECTION_SEPARATOR {
        return Err(Error::InvalidInstruction);
    }
    bytes.push(opcode);
    match instruction {
        Instruction::LoadConstant8 { cp_addr } => bytes.push(*cp_addr),
        Instruction::LoadConstant16 { cp_addr } => bytes.extend_from_slice(&cp_addr.to_be_bytes()),
        Instruction::Return => (),
    }
    Ok(())
}

fn push_line_number_table(bytes: &mut Vec<u8>, line_table: &[LineNumberEntry]) -> Result<()> {
    for entry in line_table.iter() {
        push_line_number_entry(bytes, entry)?;
    }
    Ok(())
}

fn push_line_number_entry(bytes: &mut Vec<u8>, entry: &LineNumberEntry) -> Result<()> {
    // NOTE: Truncates to 2 bytes, should be removed if code section is more than 65535 bytes
    assert!(entry.start_pc <= u16::MAX as usize);

    let start_pc = &entry.start_pc.to_be_bytes();
    bytes.extend_from_slice(&start_pc[start_pc.len() - 2..=start_pc.len() - 1]);
    bytes.extend_from_slice(&entry.line_number.to_be_bytes());
    Ok(())
}
