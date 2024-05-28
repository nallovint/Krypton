use std::mem;

use crate::bytecode::{Instruction, Klass, LineNumberEntry, Value};
use crate::bytecode::Instruction::LoadConstant;
use crate::compiler::lex::{Token, TokenStream, TokenType};

type Result<T> = std::result::Result<T, Vec<Error>>;

#[derive(Debug)]
pub enum Error {
    LexingError(Token),
    UnexpectedEndOfTokenStream,
    UnexpectedToken {
        got: Token,
        expected: TokenType,
        message: String,
    },
}

#[derive(Debug)]
pub struct Parser<I: IntoIterator<Item = Token>> {
    input: I,
    klass: Klass,
    previous: Option<Token>,
    current: Token,
    errors: Vec<Error>,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(input: I) -> Self {
        Self {
            input,
            klass: Klass::default(),
            errors: Vec::new(),
            previous: None,
            // A dummy token to start the iteration
            // Not recommended, but it's better than having an option in the struct
            // And then having to check for None everywhere even though it's impossible to be None
            // Because self.advance() is immediately called
            current: Token::dummy(),
        }
    }

    pub fn parse(mut self) -> Result<Klass> {
        self.advance()?; // Immediately parse the dummy token
        self.expression()?;
        self.consume(TokenType::Eof, "End of expression and file")?;

        // FIXME: Remove this later, temporary instruction to "print" the
        // result
        self.klass.instructions.push(Instruction::Return);

        Ok(self.klass)
    }

    fn advance(&mut self) -> Result<()> {
        let mut errors = Vec::new();
        loop {
            let token = self.input.next();

            let Some(token) = token else {
                errors.push(Error::UnexpectedEndOfTokenStream);
                break;
            };

            if matches!(token.token_type, TokenType::Err(_)) {
                errors.push(Error::LexingError(token));
                break;
            }

            let mut previous = token;
            mem::swap(&mut self.current, &mut previous);
            self.previous = Some(previous);
            break;
        }

        if errors.is_empty() { Ok(()) } else { Err(errors) }
    }

    fn consume(&mut self, token_type: TokenType, expected: &str) -> Result<()> {
        if self.current.token_type == token_type {
            self.advance()?;
            return Ok(());
        }
        return Err(vec![Error::UnexpectedToken {
            got: mem::replace(&mut self.current, Token::dummy()),
            expected: token_type,
            message: expected.to_string(),
        }]);
    }

    fn emit_instruction(&mut self, instruction: Instruction, line: u16) {
        let previous = self.klass.line_number_table.last();
        let add_entry = match previous {
            Some(entry) => entry.line_number != line,
            None => true,
        };

        if add_entry {
            self.klass.line_number_table.push(LineNumberEntry {
                start_pc: self.klass.instructions.len(),
                line_number: line,
            });
        }

        self.klass.instructions.push(instruction);
    }

    // =======================================================================
    // =======================================================================

    fn emit_constant(&mut self, value: Value) -> u16 {
        let cp_addr = self.klass.constant_pool.len() as u16;
        self.klass.constant_pool.push(value);
        cp_addr
    }

    fn number(&mut self) {
        let token = self.previous.take().expect("number() called without previous token");
        let value = value_from_token(&token.token_type);
        let cp_addr = self.emit_constant(value);
        self.emit_instruction(LoadConstant { cp_addr }, token.line);
    }

    fn grouping(&mut self) -> Result<()> {
        self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after expression")?;
        Ok(())
    }

    fn unary(&mut self) -> Result<()> {
        let token = self.previous.take().expect("unary() called without previous token");
        self.parse_precedence(Precedence::Unary)?;
        let instruction = match token.token_type {
            TokenType::Minus => Instruction::Negate,
            _ => panic!("Unsupported unary operator: {:?}", token.token_type),
        };
        self.emit_instruction(instruction, token.line);
        Ok(())
    }

    fn expression(&mut self) -> Result<()> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<()> {
        todo!()
    }
}

impl From<TokenStream> for Parser<std::vec::IntoIter<Token>> {
    fn from(input: TokenStream) -> Self {
        Self::new(input.into_iter())
    }
}

impl<I: Iterator<Item = Token>> From<I> for Parser<I> {
    fn from(value: I) -> Self {
        Self::new(value)
    }
}

fn value_from_token(token_type: &TokenType) -> Value {
    match token_type {
        TokenType::Integer(value) => {
            if (i32::MIN as i64..=i32::MAX as i64).contains(value) {
                Value::Int(*value as i32)
            } else {
                Value::Long(*value)
            }
        }
        TokenType::Float(value) => {
            if (f32::MIN as f64..=f32::MAX as f64).contains(value) {
                Value::Float(*value as f32)
            } else {
                Value::Double(*value)
            }
        }
        _ => panic!("Unsupported token type: {:?}", token_type),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Precedence {
    None = 0,
    Assignment = 1,
    Or = 2,
    And = 3,
    Equality = 4,
    Comparison = 5,
    Term = 6,
    Factor = 7,
    Unary = 8,
    Call = 9,
    Primary = 10,
}
