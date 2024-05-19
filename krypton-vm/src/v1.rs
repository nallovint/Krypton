use std::fmt::{Display, Formatter};
use std::io::Cursor;

use byteorder::{BigEndian, ReadBytesExt};
use log::debug;

const MAGIC_NUMBER: [u8; 2] = [b'K', b'r'];
const MAGIC_NUMBER_16: u16 = 0x4B52;
const VERSION: [u8; 2] = [b'V', 1];
const VERSION_16: u16 = 0x5601;
const SECTION_SEPARATOR: u8 = b'-';
const NAME_LENGTH_OFFSET: usize = 4;
// IMPORTANT: Remember to change this to sizeof(usize)
const LINE_NUMBER_ENTRY_SIZE: usize = 2;

/// 2 bytes for the magic number, 2 bytes for the version
/// 1 byte for the length of the name (0 in the minimal case)
/// 1 byte for indicating the end of the constant pool (can be empty in the minimal case)
/// 1 byte of instructions (1 instruction that is 1-byte long)
/// 1 bye for indicating the end of the bytecode
/// No line number table after it
const MINIMUM_BYTECODE_BYTES: u8 = 8;

pub type Result<T> = std::result::Result<T, Error>;

pub enum Error {
    // Compile errors
    BytecodeTooShort(u8),
    MagicNumberMismatch([u8; 2]),
    VersionMismatch([u8; 2]),
    BytecodeOutOfBounds { expected: usize, got: usize },
    MissingSectionSeparator(&'static str),
    InvalidInstruction,
    UnrecognizedOpcode(u8),
    MissingCode,
    LineNumberTableLengthNotMultipleOfEntrySize(usize),
    UnrecognizedTag(u8),
    // Runtime errors
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Error::BytecodeTooShort(got) => {
                    format!(
                        "Bytecode too short. Expected at least {} bytes, got {}",
                        MINIMUM_BYTECODE_BYTES, got
                    )
                }
                Error::MagicNumberMismatch(got) => {
                    format!(
                        "Magic number mismatch. Expected \"{}{}\" ({}), got \"{}{}\" ({})",
                        MAGIC_NUMBER[0] as char,
                        MAGIC_NUMBER[1] as char,
                        MAGIC_NUMBER_16,
                        got[0] as char,
                        got[1] as char,
                        got[0] as u16 * 256 + got[1] as u16
                    )
                }
                Error::VersionMismatch(got) => {
                    format!(
                        "Supported version is \"{}{:1}\" ({}), got \"{}{}\" ({})",
                        VERSION[0] as char,
                        VERSION[1],
                        VERSION_16,
                        got[0] as char,
                        got[1] as char,
                        got[0] as u16 * 256 + got[1] as u16
                    )
                }

                Error::BytecodeOutOfBounds { expected, got } => {
                    format!(
                        "Bytecode out of bounds. Expected at least {} bytes, got {}",
                        expected, got
                    )
                }

                Error::MissingSectionSeparator(name) => {
                    format!("Missing section separator for {}", name)
                }

                Error::InvalidInstruction => "Invalid code instruction".to_string(),

                Error::UnrecognizedOpcode(opcode) => format!("Unrecognized opcode: {}", opcode),

                Error::MissingCode => "Missing code section, must have at least one instruction!".to_string(),

                Error::LineNumberTableLengthNotMultipleOfEntrySize(length) => {
                    format!(
                        "Line number table length {} is not a multiple of entry size {}",
                        length, LINE_NUMBER_ENTRY_SIZE
                    )
                }

                Error::UnrecognizedTag(tag) => format!("Unrecognized tag: {}", tag),
            }
        )
    }
}

#[derive(Debug, Default)]
pub struct VM {
    klass: Vec<u8>,
    cp_start: usize,
    code_start: usize,
    lt_start: usize,
    lt_length: usize,
    ip: usize,
}

impl VM {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn load_and_run(&mut self, bytecode: Vec<u8>) -> Result<()> {
        self.load(&bytecode)?;

        self.klass = bytecode;
        self.ip = self.code_start;

        self.run()
    }

    fn run(&mut self) -> Result<()> {
        loop {
            debug!("IP: {:?}", self.ip);
            let opcode = self.read(self.ip);
            debug!("IP: {:?} | OP: {:?}", self.ip, opcode);
            let opcode: Opcode = Opcode::from_u8(opcode);
            self.ip += 1 + opcode.operands_bytes_length() as usize;
            match opcode {
                Opcode::Return => return Ok(()),
                Opcode::LoadConstant8 => self.load_constant8(),
                Opcode::LoadConstant16 => self.load_constant16(),
            }
        }
    }

    #[inline(always)]
    fn load_constant8(&self) {
        debug!("Loading constant 8");
        self.load_constant(self.read_u8(self.ip - 1) as u16);
    }

    #[inline(always)]
    fn load_constant16(&self) {
        debug!("Loading constant 16");
        debug!("IP: {:?}", self.ip);
        // self.load_constant(self.read(self.ip - 1));
        self.load_constant(self.klass[self.ip - 2] as u16 * 256 + self.klass[self.ip - 1] as u16);
    }

    fn load_constant(&self, offset: u16) {
        debug!("Loading constant with offset {}", offset);
        let tag = self.read_u8(self.cp_start + offset as usize);
        debug!("Tag: {:?}", tag);
        let tag = Tag::from_u8(tag);
        let value_pointer = self.cp_start + offset as usize + 1;
        match tag {
            Tag::Double => {
                let value = self.read::<f64>(value_pointer);
                debug!("Loaded double constant: {}", value);
            }
            Tag::Float => {
                let value = self.read::<f32>(value_pointer);
                debug!("Loaded float constant: {}", value);
            }
            Tag::Long => {
                let value = self.read::<i64>(value_pointer);
                debug!("Loaded long constant: {}", value);
            }

            Tag::Int => {
                let value = self.read::<i32>(value_pointer);
                debug!("Loaded int constant: {}", value);
            }
        }
    }

    fn load(&mut self, bytecode: &[u8]) -> Result<()> {
        if bytecode.len() < MINIMUM_BYTECODE_BYTES as usize {
            return Err(Error::BytecodeTooShort(bytecode.len() as u8));
        }
        if bytecode[0..2] != MAGIC_NUMBER {
            return Err(Error::MagicNumberMismatch([bytecode[0], bytecode[1]]));
        }
        if bytecode[2..4] != VERSION {
            return Err(Error::VersionMismatch([bytecode[2], bytecode[3]]));
        }

        let name_length = bytecode[NAME_LENGTH_OFFSET] as usize;

        // NOTE Unsafe to index for anything under this comment

        // let start_name = NAME_LENGTH_OFFSET + 1;
        // let end_name = start_name + name_length;
        // let name = &bytecode.get(start_name..end_name);

        self.cp_start = NAME_LENGTH_OFFSET + name_length + 1;
        let mut pointer = self.cp_start;
        if bytecode[self.cp_start] != SECTION_SEPARATOR {
            loop {
                let Some(&value) = bytecode.get(pointer) else {
                    return Err(Error::MissingSectionSeparator("constant pool"));
                };
                if value == SECTION_SEPARATOR {
                    break;
                }
                pointer += 1 + Tag::try_from(value)?.len() as usize;
            }
        }

        pointer += 1;
        self.code_start = pointer;
        let opcode = bytecode.get(pointer);
        let Some(&opcode) = opcode else {
            return Err(Error::MissingCode);
        };
        let mut opcode = opcode;
        loop {
            pointer += Opcode::try_from(opcode)?.operands_bytes_length() as usize + 1;
            opcode = match bytecode.get(pointer) {
                Some(&opcode) => opcode,
                None => return Err(Error::InvalidInstruction),
            };
            if opcode == SECTION_SEPARATOR {
                break;
            }
            // TODO: Add "static analysis" to check instructions, including:
            // - All "LoadConstant" instructions have a valid constant pool index
        }

        pointer += 1;
        let length = bytecode.len() - pointer;
        if length != 0 {
            if length % LINE_NUMBER_ENTRY_SIZE != 0 {
                return Err(Error::LineNumberTableLengthNotMultipleOfEntrySize(length));
            }
            self.lt_length = length / LINE_NUMBER_ENTRY_SIZE;
            // TODO: Check validity of line number table, including:
            // - Offsets that point to a valid instruction (not out of bounds)
            // - Line numbers are not 0
            // - Line numbers are sorted based on offset, for efficient binary search
        } else {
            self.lt_start = pointer;
            self.lt_length = 0;
        }

        Ok(())
    }

    #[inline]
    fn read_u8(&self, offset: usize) -> u8 {}
}

#[repr(u8)]
enum Opcode {
    LoadConstant8 = 0b_0110_0011,  // 'c'
    LoadConstant16 = 0b_0100_0011, // 'C'
    Return = 0b_0111_0010,         // 'r'
}

impl Opcode {
    // pub fn operands_count(&self) -> u8 {
    //     match self {
    //         Opcode::LoadConstant8 => 1,
    //         Opcode::LoadConstant16 => 1,
    //         Opcode::Return => 0,
    //     }
    // }

    #[inline]
    pub fn operands_bytes_length(&self) -> u8 {
        match self {
            Opcode::LoadConstant8 => 1,
            Opcode::LoadConstant16 => 2,
            Opcode::Return => 0,
        }
    }

    #[inline]
    pub fn from_u8(value: u8) -> Self {
        match value {
            0b_0110_0011 => Opcode::LoadConstant8,
            0b_0100_0011 => Opcode::LoadConstant16,
            0b_0111_0010 => Opcode::Return,
            _value => panic!("Unrecognized opcode: {}", _value),
        }
    }
}

impl TryFrom<u8> for Opcode {
    type Error = Error;
    fn try_from(value: u8) -> Result<Self> {
        Ok(match value {
            0b_0110_0011 => Opcode::LoadConstant8,
            0b_0100_0011 => Opcode::LoadConstant16,
            0b_0111_0010 => Opcode::Return,
            _ => return Err(Error::UnrecognizedOpcode(value)),
        })
    }
}

#[repr(u8)]
enum Tag {
    Double = 0b_0100_0110, // 'F'
    Float = 0b_0110_0110,  // 'f'
    Long = 0b_0100_1001,   // 'I'
    Int = 0b_0110_1001,    // 'i'
}

impl Tag {
    pub fn len(&self) -> u8 {
        match self {
            Tag::Double => 8,
            Tag::Float => 4,
            Tag::Long => 8,
            Tag::Int => 4,
        }
    }

    #[inline(always)]
    pub fn from_u8(value: u8) -> Self {
        match value {
            0b_0100_0110 => Tag::Double,
            0b_0110_0110 => Tag::Float,
            0b_0100_1001 => Tag::Long,
            0b_0110_1001 => Tag::Int,
            _ => panic!("Unrecognized tag: {}", value),
        }
    }
}

impl TryFrom<u8> for Tag {
    type Error = Error;
    fn try_from(value: u8) -> Result<Self> {
        Ok(match value {
            0b_0100_0110 => Tag::Double,
            0b_0110_0110 => Tag::Float,
            0b_0100_1001 => Tag::Long,
            0b_0110_1001 => Tag::Int,
            _ => return Err(Error::UnrecognizedTag(value)),
        })
    }
}
