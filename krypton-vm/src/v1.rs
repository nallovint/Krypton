use std::fmt::{Display, Formatter};

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
            }
        )
    }
}

#[derive(Debug, Default)]
pub struct VM {
    klass: Vec<u8>,
    cpp: usize,        // [C]onstant [P]ool [P]ointer
    cp: usize,         // [C]ode [P]ointer
    ltp: usize,        // [L]ine Number [T]able [P]ointer
    lnt_length: usize, // [L]ine Number [T]able [L]ength
    ip: usize,         // [I]nstruction [P]ointer
}

impl VM {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn run(&mut self, bytecode: Vec<u8>) -> Result<()> {
        self.verify(&bytecode)?;

        self.klass = bytecode;
        self.ip = self.cp;

        Ok(())
    }

    fn verify(&mut self, bytecode: &[u8]) -> Result<()> {
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

        self.cpp = NAME_LENGTH_OFFSET + name_length + 1;
        // TODO: Check that the constant pool is valid, including:
        // - All "tags" are valid tag types
        // - All "LoadConstant" instructions have a valid constant pool index

        // FIXME Constant pool value can contain a SECTION_SEPARATOR byte
        // FIXME This will cause the constant pool to end and it'll try to
        // FIXME read the next byte as an opcode to possibly execute
        let mut cp = self.cpp;
        if bytecode[self.cpp] != SECTION_SEPARATOR {
            loop {
                let Some(&value) = bytecode.get(cp) else {
                    return Err(Error::MissingSectionSeparator(""));
                };
                if value == SECTION_SEPARATOR {
                    break;
                }
                cp += 1;
            }
        }
        self.cp = cp;

        let mut ip = self.cp;
        let opcode = bytecode.get(ip);
        let Some(&opcode) = opcode else {
            return Err(Error::MissingCode);
        };
        let mut opcode = opcode;
        loop {
            ip += Opcode::try_from(opcode)?.operands_bytes_length() as usize + 1;
            opcode = match bytecode.get(ip) {
                Some(&opcode) => opcode,
                None => return Err(Error::InvalidInstruction),
            };
            if opcode == SECTION_SEPARATOR {
                break;
            }
        }

        let ltp = ip + 1;
        let length = bytecode.len() - ltp;
        if length != 0 {
            if length % LINE_NUMBER_ENTRY_SIZE != 0 {
                return Err(Error::LineNumberTableLengthNotMultipleOfEntrySize(length));
            }
            self.lnt_length = length / LINE_NUMBER_ENTRY_SIZE;
            // TODO: Check validity of line number table, including:
            // - Offsets that point to a valid instruction (not out of bounds)
            // - Line numbers are not 0
            // - Line numbers are sorted based on offset, for efficient binary search
        } else {
            self.ltp = ip;
            self.lnt_length = 0;
        }

        Ok(())
    }
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

    pub fn operands_bytes_length(&self) -> u8 {
        match self {
            Opcode::LoadConstant8 => 1,
            Opcode::LoadConstant16 => 2,
            Opcode::Return => 0,
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
