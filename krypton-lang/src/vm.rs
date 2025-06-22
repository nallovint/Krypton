use std::fmt::{Display, Formatter};
use std::mem::MaybeUninit;
use std::collections::HashMap;

use crate::bytecode;
use crate::bytecode::{decode, Instruction, Klass, Value};

const MAX_STACK_SIZE: u16 = StackPointer::MAX as u16;
type StackPointer = u8;
type InstructionPointer = u32;
type Stack<T> = [MaybeUninit<T>; MAX_STACK_SIZE as usize];

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    // Compile errors
    DecodeError(bytecode::Error),
    // Runtime errors
    UndefinedVariable(String),
    TypeError(String),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::DecodeError(error) => error.to_string(),
            Self::UndefinedVariable(name) => format!("Undefined variable '{}'", name),
            Self::TypeError(msg) => format!("Type error: {}", msg),
        })
    }
}

#[derive(Debug)]
pub struct VM {
    klass: Klass,
    ip: InstructionPointer,
    stack: Stack<Value>,
    sp: StackPointer,
    globals: HashMap<String, Value>,
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
            globals: HashMap::new(),
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
                Instruction::DefineGlobal { cp_addr } => self.define_global(cp_addr)?,
                Instruction::GetGlobal { cp_addr } => self.get_global(cp_addr)?,
                Instruction::SetGlobal { cp_addr } => self.set_global(cp_addr)?,
                Instruction::Jump { offset } => self.jump(offset),
                Instruction::JumpIfFalse { offset } => self.jump_if_false(offset)?,
                Instruction::EqualEqual => self.equal_equal(),
                Instruction::BangEqual => self.bang_equal(),
                Instruction::Less => self.less(),
                Instruction::LessEqual => self.less_equal(),
                Instruction::Greater => self.greater(),
                Instruction::GreaterEqual => self.greater_equal(),
                Instruction::Print => self.print_value(),
            }
        }
        Ok(())
    }

    #[inline]
    fn load_constant(&mut self, offset: u16) {
        let constant = unsafe { (*self.klass.constant_pool.get_unchecked(offset as usize)).clone() };
        self.push(constant);
    }

    #[inline]
    fn return_instruction(&mut self) {
        if self.sp > 0 {
            println!("{}", self.pop());
        }
        // else: do nothing
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

    #[inline]
    fn define_global(&mut self, cp_addr: u16) -> Result<()> {
        let name = self.get_constant_string(cp_addr)?;
        let value = self.pop();
        self.globals.insert(name, value);
        Ok(())
    }

    #[inline]
    fn get_global(&mut self, cp_addr: u16) -> Result<()> {
        let name = self.get_constant_string(cp_addr)?;
        let value = self.globals.get(&name)
            .ok_or_else(|| Error::UndefinedVariable(name.clone()))?
            .clone();
        self.push(value);
        Ok(())
    }

    #[inline]
    fn set_global(&mut self, cp_addr: u16) -> Result<()> {
        let name = self.get_constant_string(cp_addr)?;
        let value = self.pop();
        if self.globals.contains_key(&name) {
            self.globals.insert(name, value);
            Ok(())
        } else {
            Err(Error::UndefinedVariable(name))
        }
    }

    #[inline]
    fn get_constant_string(&self, cp_addr: u16) -> Result<String> {
        let value = unsafe { (*self.klass.constant_pool.get_unchecked(cp_addr as usize)).clone() };
        match value {
            Value::String(s) => Ok(s),
            _ => panic!("Expected string constant"),
        }
    }

    fn push(&mut self, value: Value) {
        assert_ne!(self.sp as u16, MAX_STACK_SIZE, "Stack overflow");
        unsafe {
            let slot = self.stack.get_unchecked_mut(self.sp as usize);
            slot.write(value);
        }
        self.sp += 1;
    }

    fn pop(&mut self) -> Value {
        assert_ne!(self.sp as u16, 0, "Stack underflow");
        self.sp -= 1;
        unsafe {
            let slot = self.stack.get_unchecked_mut(self.sp as usize);
            slot.assume_init_read()
        }
    }

    fn debug_trace(&self, instruction: Instruction) -> String {
        format!("{}\n{}\n", self.debug_stack_trace(), self.debug_instruction_trace(instruction))
    }

    fn debug_stack_trace(&self) -> String {
        let mut stack_strs = Vec::new();
        for i in 0..self.sp as usize {
            let value = unsafe { self.stack.get_unchecked(i).assume_init_ref() };
            stack_strs.push(format!("[ {:^11} ] # {}", format!("{}", value), i));
        }
        format!(
            "==== Stack ====\n{}\n---------------",
            stack_strs.join("\n")
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

    #[inline]
    fn jump(&mut self, offset: u16) {
        self.ip = (self.ip as i32 + offset as i32) as u32;
    }

    #[inline]
    fn jump_if_false(&mut self, offset: u16) -> Result<()> {
        let value = self.pop();
        let condition = match value {
            Value::Int(0) => false,
            Value::Int(1) => true,
            _ => return Err(Error::TypeError("Expected boolean value".to_owned())),
        };
        if !condition {
            self.jump(offset);
        }
        Ok(())
    }

    #[inline]
    fn equal_equal(&mut self) {
        let b = self.pop();
        let a = self.pop();
        let result = match (a, b) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Double(a), Value::Double(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            _ => false,
        };
        self.push(Value::Int(if result { 1 } else { 0 }));
    }

    #[inline]
    fn bang_equal(&mut self) {
        let b = self.pop();
        let a = self.pop();
        let result = match (a, b) {
            (Value::Int(a), Value::Int(b)) => a != b,
            (Value::Float(a), Value::Float(b)) => a != b,
            (Value::Double(a), Value::Double(b)) => a != b,
            (Value::String(a), Value::String(b)) => a != b,
            _ => true,
        };
        self.push(Value::Int(if result { 1 } else { 0 }));
    }

    #[inline]
    fn less(&mut self) {
        let b = self.pop();
        let a = self.pop();
        let result = match (a, b) {
            (Value::Int(a), Value::Int(b)) => a < b,
            (Value::Float(a), Value::Float(b)) => a < b,
            (Value::Double(a), Value::Double(b)) => a < b,
            _ => {
                let _ : std::result::Result<(), Error> = Err(Error::TypeError("Cannot compare non-numeric values".to_owned()));
                return;
            },
        };
        self.push(Value::Int(if result { 1 } else { 0 }));
    }

    #[inline]
    fn less_equal(&mut self) {
        let b = self.pop();
        let a = self.pop();
        let result = match (a, b) {
            (Value::Int(a), Value::Int(b)) => a <= b,
            (Value::Float(a), Value::Float(b)) => a <= b,
            (Value::Double(a), Value::Double(b)) => a <= b,
            _ => {
                let _ : std::result::Result<(), Error> = Err(Error::TypeError("Cannot compare non-numeric values".to_owned()));
                return;
            },
        };
        self.push(Value::Int(if result { 1 } else { 0 }));
    }

    #[inline]
    fn greater(&mut self) {
        let b = self.pop();
        let a = self.pop();
        let result = match (a, b) {
            (Value::Int(a), Value::Int(b)) => a > b,
            (Value::Float(a), Value::Float(b)) => a > b,
            (Value::Double(a), Value::Double(b)) => a > b,
            _ => {
                let _ : std::result::Result<(), Error> = Err(Error::TypeError("Cannot compare non-numeric values".to_owned()));
                return;
            },
        };
        self.push(Value::Int(if result { 1 } else { 0 }));
    }

    #[inline]
    fn greater_equal(&mut self) {
        let b = self.pop();
        let a = self.pop();
        let result = match (a, b) {
            (Value::Int(a), Value::Int(b)) => a >= b,
            (Value::Float(a), Value::Float(b)) => a >= b,
            (Value::Double(a), Value::Double(b)) => a >= b,
            _ => {
                let _ : std::result::Result<(), Error> = Err(Error::TypeError("Cannot compare non-numeric values".to_owned()));
                return;
            },
        };
        self.push(Value::Int(if result { 1 } else { 0 }));
    }

    #[inline]
    fn print_value(&mut self) {
        let value = self.pop();
        println!("{}", value);
    }
}

impl Drop for VM {
    fn drop(&mut self) {
        // Only drop initialized elements
        for i in 0..self.sp as usize {
            unsafe {
                self.stack.get_unchecked_mut(i).assume_init_drop();
            }
        }
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}
