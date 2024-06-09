use std::fmt::{Display, Formatter};
use std::mem::MaybeUninit;

use crate::bytecode;
use crate::bytecode::{decode, Instruction, Klass, Value};

const MAX_STACK_SIZE: u16 = StackPointer::MAX as u16;
type StackPointer = u8;
type InstructionPointer = u32;
type Stack<T> = [T; MAX_STACK_SIZE as usize];

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    // Compile errors
    DecodeError(bytecode::Error),
    // Runtime errors
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::DecodeError(error) => error.to_string(),
        })
    }
}

#[derive(Debug)]
pub struct VM {
    klass: Klass,
    ip: InstructionPointer,
    stack: Stack<Value>,
    sp: StackPointer,
}

impl VM {
    #[must_use]
    pub fn new() -> Self {
        Self {
            // SAFETY: the pop and push methods guarantee safety
            stack: unsafe { MaybeUninit::assume_init(MaybeUninit::uninit()) },
            klass: Klass::default(),
            ip: 0,
            sp: 0,
        }
    }

    #[allow(clippy::missing_errors_doc)]
    pub fn load_bytecode_and_run(&mut self, bytecode: &[u8]) -> Result<()> {
        self.load_bytecode(bytecode)?;
        self.run()
    }

    #[allow(clippy::missing_errors_doc)]
    pub fn load_and_run(&mut self, klass: &Klass) -> Result<()> {
        self.load(klass)?;
        self.run()
    }

    #[allow(clippy::missing_errors_doc)]
    pub fn load_bytecode(&mut self, bytecode: &[u8]) -> Result<()> {
        self.klass = decode(bytecode).map_err(Error::DecodeError)?;
        self.ip = 0;
        self.sp = 0;
        Ok(())
    }

    #[allow(clippy::missing_errors_doc)]
    pub fn load(&mut self, klass: &Klass) -> Result<()> {
        self.klass = klass.clone();
        self.ip = 0;
        self.sp = 0;
        Ok(())
    }

    #[allow(clippy::missing_errors_doc)]
    #[allow(clippy::missing_panics_doc)]
    pub fn run(&mut self) -> Result<()> {
        while (self.ip as usize) < self.klass.instructions.len() {
            // SAFETY:
            //  The while loop above ensures that the index is always in bounds
            //  so it's safe to index into the instructions array without bound
            // checks
            let instruction = unsafe { *self.klass.instructions.get_unchecked(self.ip as usize) };
            debug!("{}", self.debug_trace(instruction));
            self.ip += 1;
            match instruction {
                Instruction::LoadConstant { cp_addr } => self.load_constant(cp_addr),
                Instruction::Return => self.return_instruction(),
                Instruction::Negate => self.negate_value(),
                Instruction::Add => self.add_value(),
                Instruction::Subtract => self.subtract_value(),
                Instruction::Multiply => self.multiply_value(),
                Instruction::Divide => self.divide_value(),
            }
        }
        Ok(())
    }

    #[inline]
    fn load_constant(&mut self, offset: u16) {
        //SAFETY:
        // Assumes the bytecode is valid and never tries to access an out of
        // bounds index so it's safe to index into the constant pool
        // If the bytecode may be malformed, this function is **undefined
        // behavior**
        let constant = unsafe { *self.klass.constant_pool.get_unchecked(offset as usize) };
        self.push(constant);
    }

    #[inline]
    fn return_instruction(&mut self) {
        println!("{}", self.pop());
    }

    #[inline]
    fn negate_value(&mut self) {
        let value = self.pop();
        self.push(-value);
    }

    #[inline]
    fn add_value(&mut self) {
        let b = self.pop();
        let a = self.pop();
        self.push(a + b);
    }

    #[inline]
    fn subtract_value(&mut self) {
        let b = self.pop();
        let a = self.pop();
        self.push(a - b);
    }

    #[inline]
    fn multiply_value(&mut self) {
        let b = self.pop();
        let a = self.pop();
        self.push(a * b);
    }

    #[inline]
    fn divide_value(&mut self) {
        let b = self.pop();
        let a = self.pop();
        self.push(a / b);
    }

    fn push(&mut self, value: Value) {
        // NOTE: Uses MAX_STACK_SIZE because sp points to the next free slot
        assert_ne!(self.sp as u16, MAX_STACK_SIZE, "Stack overflow");
        // SAFETY: the sp is being bound checked above
        *unsafe { self.stack.get_unchecked_mut(self.sp as usize) } = value;
        self.sp += 1;
    }

    fn pop(&mut self) -> Value {
        assert_ne!(self.sp as u16, 0, "Stack underflow");
        self.sp -= 1;
        // SAFETY: The sp is being bound checked above
        unsafe { std::ptr::read(self.stack.get_unchecked(self.sp as usize)) }
    }

    fn debug_trace(&self, instruction: Instruction) -> String {
        format!("{}\n{}\n", self.debug_stack_trace(), self.debug_instruction_trace(instruction))
    }

    fn debug_stack_trace(&self) -> String {
        format!(
            "==== Stack ====\n{}\n---------------",
            self.stack
                .iter()
                .enumerate()
                .filter(|(i, _)| *i < self.sp as usize)
                .map(|(i, value)| format!("[ {:^11} ] # {}", format!("{value}"), i))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }

    fn debug_instruction_trace(&self, instruction: Instruction) -> String {
        format!(
            "IP: {}, OP: {} '{}' # {}",
            self.ip,
            instruction.opcode() as u8,
            instruction.opcode() as u8 as char,
            instruction
        )
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}
