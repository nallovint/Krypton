use std::cmp::PartialOrd;
use std::mem;
use std::ops::Add;

use crate::bytecode::{Instruction, Klass, LineNumberEntry, Value};
use crate::bytecode::Instruction::LoadConstant;
use crate::compiler::lex::{Token, TokenStream, TokenType};

pub type Result<T> = std::result::Result<T, Vec<Error>>;

#[derive(Debug)]
pub enum Error {
    LexingError(Token),
    UnexpectedEndOfTokenStream,
    UnexpectedToken {
        got: Token,
        expected: TokenType,
        message: String,
    },
    ExpectedExpression {
        got: Token,
        message: String,
    },
}

#[derive(Debug)]
pub struct Parser<I: IntoIterator<Item = Token>> {
    input: I,
    klass: Klass,
    previous: Option<Token>,
    current: Token,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(input: I) -> Self {
        Self {
            input,
            klass: Klass::default(),
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

        // FIXME
        //  Remove this later, temp instruction to "print" the result
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
                continue;
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
        Err(vec![Error::UnexpectedToken {
            got: mem::replace(&mut self.current, Token::dummy()),
            expected: token_type,
            message: expected.to_string(),
        }])
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

    fn binary(&mut self) -> Result<()> {
        let token = self.previous.take().expect("binary() called without previous token");

        self.parse_precedence(Precedence::from_token(&token.token_type) + 1)?;

        let instruction = match token.token_type {
            TokenType::Plus => Instruction::Add,
            TokenType::Minus => Instruction::Subtract,
            TokenType::Star => Instruction::Multiply,
            TokenType::Slash => Instruction::Divide,
            _ => panic!("Unsupported binary operator: {:?}", token.token_type),
        };
        self.emit_instruction(instruction, token.line);
        Ok(())
    }

    fn expression(&mut self) -> Result<()> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<()> {
        self.advance()?;

        let prev = self
            .previous
            .take()
            .expect("parse_precedence() called without previous token");
        match prev.token_type {
            TokenType::LeftParen => self.grouping()?,
            TokenType::Minus => self.unary()?,
            TokenType::Integer(_) => self.number(),
            TokenType::Float(_) => self.number(),
            _ => {
                return Err(vec![Error::ExpectedExpression {
                    got: prev,
                    message: "Expected expression".to_string(),
                }]);
            }
        }

        while precedence <= Precedence::from_token(&self.current.token_type) {
            self.advance()?;
            let prev = self
                .previous
                .take()
                .expect("parse_precedence() called without previous token, inside infix loop");

            match prev.token_type {
                TokenType::Plus => self.binary()?,
                TokenType::Minus => self.binary()?,
                TokenType::Star => self.binary()?,
                TokenType::Slash => self.binary()?,
                _ => {
                    return Err(vec![Error::ExpectedExpression {
                        got: prev,
                        message: "Expected expression".to_string(),
                    }]);
                }
            }
        }

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

/// When adding a new precedence, make sure to add it to
/// [here](Precedence::from)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Precedence {
    None = 0,
    Assignment = 1, // =
    Or = 2,         // ||
    And = 3,        // &&
    Equality = 4,   // ==, !=
    Comparison = 5, // <, >, <=, >=
    Term = 6,       // +, -
    Factor = 7,     // *, /
    Unary = 8,      // !, -
    Call = 9,       // . ()
    Primary = 10,
}

impl Precedence {
    fn from_token(token_type: &TokenType) -> Self {
        match token_type {
            TokenType::LeftParen => Precedence::None,
            TokenType::RightParen => Precedence::None,
            TokenType::LeftBrace => Precedence::None,
            TokenType::RightBrace => Precedence::None,
            TokenType::LeftBracket => Precedence::None,
            TokenType::RightBracket => Precedence::None,
            TokenType::Comma => Precedence::None,
            TokenType::Dot => Precedence::None,
            TokenType::Minus => Precedence::Term,
            TokenType::Plus => Precedence::Term,
            TokenType::Semicolon => Precedence::None,
            TokenType::Colon => Precedence::None,
            TokenType::Slash => Precedence::Factor,
            TokenType::Star => Precedence::Factor,
            TokenType::Bang => Precedence::None,
            TokenType::BangEqual => Precedence::None,
            TokenType::Equal => Precedence::None,
            TokenType::EqualEqual => Precedence::None,
            TokenType::Greater => Precedence::None,
            TokenType::GreaterEqual => Precedence::None,
            TokenType::Less => Precedence::None,
            TokenType::LessEqual => Precedence::None,
            TokenType::Identifier(_) => Precedence::None,
            TokenType::String(_) => Precedence::None,
            TokenType::Character(_) => Precedence::None,
            TokenType::Integer(_) => Precedence::None,
            TokenType::Float(_) => Precedence::None,
            TokenType::Class => Precedence::None,
            TokenType::If => Precedence::None,
            TokenType::Else => Precedence::None,
            TokenType::True => Precedence::None,
            TokenType::False => Precedence::None,
            TokenType::While => Precedence::None,
            TokenType::For => Precedence::None,
            TokenType::Fn => Precedence::None,
            TokenType::Null => Precedence::None,
            TokenType::Print => Precedence::None,
            TokenType::Return => Precedence::None,
            TokenType::Super => Precedence::None,
            TokenType::This => Precedence::None,
            TokenType::Var => Precedence::None,
            TokenType::Eof => Precedence::None,
            TokenType::Err(_) => Precedence::None,
        }
    }
}

impl From<u8> for Precedence {
    fn from(precedence: u8) -> Self {
        match precedence {
            0 => Precedence::None,
            1 => Precedence::Assignment,
            2 => Precedence::Or,
            3 => Precedence::And,
            4 => Precedence::Equality,
            5 => Precedence::Comparison,
            6 => Precedence::Term,
            7 => Precedence::Factor,
            8 => Precedence::Unary,
            9 => Precedence::Call,
            10 => Precedence::Primary,
            _ => panic!("Unsupported precedence: {}", precedence),
        }
    }
}

impl Add<u8> for Precedence {
    type Output = Self;

    fn add(self, rhs: u8) -> Self::Output {
        Precedence::from(self as u8 + rhs)
    }
}
