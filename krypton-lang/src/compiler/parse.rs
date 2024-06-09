use std::cmp::PartialOrd;
use std::mem;
use std::ops::Add;

use crate::bytecode::Instruction::LoadConstant;
use crate::bytecode::{Instruction, Klass, LineNumberEntry, Value};
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
    #[allow(clippy::missing_panics_doc)]
    pub fn new(mut input: I) -> Self {
        let current = input.next().expect("Parser::new() called with empty input");
        Self {
            input,
            klass: Klass::default(),
            previous: None,
            current,
        }
    }

    #[allow(clippy::missing_errors_doc)]
    pub fn parse(mut self) -> Result<Klass> {
        self.expression(None)?;
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
                continue;
            }

            let mut previous = token;
            mem::swap(&mut self.current, &mut previous);
            self.previous = Some(previous);
            break;
        }

        if errors.is_empty() {
            // debug!("Previous token: {:?}", self.previous);
            // debug!("Current token: {:?}", self.current);
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn consume(&mut self, token_type: TokenType, expected: &str) -> Result<()> {
        if self.current.token_type == token_type {
            // Prevents crash when asserting EOF (bcz after EOF, calling advance()
            // will error)
            if token_type != TokenType::Eof {
                self.advance()?;
            }
            return Ok(());
        }
        Err(vec![Error::UnexpectedToken {
            got: mem::replace(&mut self.current, Token::dummy()),
            expected: token_type,
            message: expected.to_owned(),
        }])
    }

    fn emit_instruction(&mut self, instruction: Instruction, line: u16) {
        debug!("Emitting instruction: {instruction:?}");
        let previous = self.klass.line_number_table.last();
        let add_entry = previous.map_or(true, |entry| entry.line_number != line);

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
        debug!("Emitting constant: {value:?}");
        #[allow(clippy::cast_possible_truncation)]
        let cp_addr = self.klass.constant_pool.len() as u16;
        self.klass.constant_pool.push(value);
        cp_addr
    }

    fn number(&mut self) {
        debug!("Parsing number");
        let token = self
            .previous
            .clone()
            .expect("number() called without previous token");
        let value = value_from_token(&token.token_type);
        let cp_addr = self.emit_constant(value);
        self.emit_instruction(LoadConstant { cp_addr }, token.line);
    }

    fn grouping(&mut self) -> Result<()> {
        self.expression(None)?;
        self.consume(TokenType::RightParen, "Expect ')' after expression")?;
        Ok(())
    }

    fn unary(&mut self) -> Result<()> {
        let token = self
            .previous
            .clone()
            .expect("unary() called without previous token");

        self.expression(Some(Precedence::Unary))?;

        let instruction = match token.token_type {
            TokenType::Minus => Instruction::Negate,
            _ => panic!("Unsupported unary operator: {:?}", token.token_type),
        };
        self.emit_instruction(instruction, token.line);
        Ok(())
    }

    fn binary(&mut self) -> Result<()> {
        let token = self
            .previous
            .clone()
            .expect("binary() called without previous token");

        self.expression(Some(Precedence::from_token(&token.token_type) + 1))?;

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

    fn expression(&mut self, precedence: Option<Precedence>) -> Result<()> {
        let precedence = precedence.unwrap_or(Precedence::Assignment);
        self.advance()?;

        debug!("Parsing expression with precedence: {precedence:?}");

        let prev = self
            .previous
            .clone()
            .expect("parse_precedence() called without previous token");

        debug!("Previous token: {prev:?}");

        match prev.token_type {
            TokenType::LeftParen => self.grouping()?,
            TokenType::Minus => self.unary()?,
            TokenType::Integer(_) | TokenType::Float(_) => self.number(),
            _ => {
                return Err(vec![Error::ExpectedExpression {
                    got: prev,
                    message: "Expected expression".to_owned(),
                }]);
            }
        }

        while precedence <= Precedence::from_token(&self.current.token_type) {
            self.advance()?;
            let prev = self
                .previous
                .clone()
                .expect("parse_precedence() called without previous token, inside infix loop");

            match prev.token_type {
                TokenType::Plus => self.binary()?,
                TokenType::Minus => self.binary()?,
                TokenType::Star => self.binary()?,
                TokenType::Slash => self.binary()?,
                _ => {
                    return Err(vec![Error::ExpectedExpression {
                        got: prev,
                        message: "Expected expression".to_owned(),
                    }]);
                }
            }

            if self.current.token_type == TokenType::Eof {
                break;
            }
        }

        Ok(())
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
    // debug!("Value from token: {token_type:?}");
    match token_type {
        TokenType::Integer(value) => i32::try_from(*value).map_or(Value::Long(*value), Value::Int),
        TokenType::Float(value) => {
            #[allow(clippy::cast_possible_truncation)]
            let f32_value = *value as f32;
            #[allow(clippy::float_cmp)]
            if f32_value as f64 == *value {
                Value::Float(f32_value)
            } else {
                Value::Double(*value)
            }
        }
        _ => panic!("Unsupported token type: {token_type:?}"),
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
    const fn from_token(token_type: &TokenType) -> Self {
        match token_type {
            TokenType::Minus | TokenType::Plus => Precedence::Term,
            TokenType::Slash | TokenType::Star => Precedence::Factor,
            TokenType::LeftParen
            | TokenType::RightParen
            | TokenType::LeftBrace
            | TokenType::RightBrace
            | TokenType::LeftBracket
            | TokenType::RightBracket
            | TokenType::Comma
            | TokenType::Dot
            | TokenType::Semicolon
            | TokenType::Colon
            | TokenType::Bang
            | TokenType::BangEqual
            | TokenType::Equal
            | TokenType::EqualEqual
            | TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Less
            | TokenType::LessEqual
            | TokenType::Identifier(_)
            | TokenType::String(_)
            | TokenType::Character(_)
            | TokenType::Integer(_)
            | TokenType::Float(_)
            | TokenType::Class
            | TokenType::If
            | TokenType::Else
            | TokenType::True
            | TokenType::False
            | TokenType::While
            | TokenType::For
            | TokenType::Fn
            | TokenType::Null
            | TokenType::Print
            | TokenType::Return
            | TokenType::Super
            | TokenType::This
            | TokenType::Var
            | TokenType::Eof
            | TokenType::Err(_) => Precedence::None,
        }
    }
}

impl From<u8> for Precedence {
    fn from(precedence: u8) -> Self {
        match precedence {
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
            _ => Precedence::None, // Also when 0
        }
    }
}

impl Add<u8> for Precedence {
    type Output = Self;

    fn add(self, rhs: u8) -> Self::Output {
        Precedence::from(self as u8 + rhs)
    }
}
