use std::fmt::Display;

use unescaper::unescape;

pub enum Error {
    UnrecognizedCharacter(char),
    UnterminatedString,
    UnterminatedCharacter,
    EscapeError(unescaper::Error),
    NotSingleCharacter(String, usize),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Error::UnrecognizedCharacter(c) => format!("Unrecognized character '{}'", c),
                Error::UnterminatedString => "Unterminated string".to_string(),
                Error::UnterminatedCharacter => "Unterminated character".to_string(),
                Error::EscapeError(e) => format!("Escape error: {}", e),
                Error::NotSingleCharacter(s, n) => format!(
                    "Single quotes must contain only 1 character, '{}' has {} characters",
                    s, n
                ),
            }
        )
    }
}

pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Colon,
    Slash,
    Star,

    //One or more character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    //Literals
    Identifier(String),
    String(String),
    Character(char),
    Integer(i64),
    Float(f64),

    // Keywords
    Class,
    If,
    Else,
    True,
    False,
    While,
    For,
    Fn,
    Null,
    Print,
    Return,
    Super,
    This,
    Var,

    //
    EOF,
    Err(Error),
}

pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: u16,
    pub column: u16,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: u16, column: u16) -> Self {
        Self {
            token_type,
            lexeme,
            line,
            column,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "'{}' ({}:{}::{})",
            self.lexeme, self.line, self.column, self.token_type
        )
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenType::EOF => "EOF".to_string(),
                TokenType::Err(error) => format!("Err({})", error),
                TokenType::LeftParen => "(".to_string(),
                TokenType::LeftBrace => "{".to_string(),
                TokenType::LeftBracket => "[".to_string(),
                TokenType::RightParen => ")".to_string(),
                TokenType::RightBrace => "}".to_string(),
                TokenType::RightBracket => "]".to_string(),
                TokenType::Comma => ",".to_string(),
                TokenType::Dot => ".".to_string(),
                TokenType::Minus => "-".to_string(),
                TokenType::Plus => "+".to_string(),
                TokenType::Semicolon => ";".to_string(),
                TokenType::Colon => ":".to_string(),
                TokenType::Slash => "/".to_string(),
                TokenType::Star => "*".to_string(),
                TokenType::Bang => "!".to_string(),
                TokenType::BangEqual => "!=".to_string(),
                TokenType::Equal => "=".to_string(),
                TokenType::EqualEqual => "==".to_string(),
                TokenType::Greater => ">".to_string(),
                TokenType::GreaterEqual => ">=".to_string(),
                TokenType::Less => "<".to_string(),
                TokenType::LessEqual => "<=".to_string(),
                TokenType::Identifier(identifier) => identifier.to_string(),
                TokenType::String(string) => format!("\"{}\"", string),
                TokenType::Character(character) => format!("'{}'", character),
                TokenType::Integer(integer) => integer.to_string(),
                TokenType::Float(float) => float.to_string(),
                TokenType::Class => "class".to_string(),
                TokenType::If => "if".to_string(),
                TokenType::Else => "else".to_string(),
                TokenType::True => "true".to_string(),
                TokenType::False => "false".to_string(),
                TokenType::While => "while".to_string(),
                TokenType::For => "for".to_string(),
                TokenType::Fn => "fn".to_string(),
                TokenType::Null => "null".to_string(),
                TokenType::Print => "print".to_string(),
                TokenType::Return => "return".to_string(),
                TokenType::Super => "super".to_string(),
                TokenType::This => "this".to_string(),
                TokenType::Var => "var".to_string(),
            }
        )
    }
}

pub struct Lexer {
    source: Vec<char>,
    start: usize,
    current: usize,
    line: u16,
    column: u16,
    stop: bool,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            start: 0,
            current: 0,
            line: 1,
            column: 0,
            stop: false,
        }
    }

    pub fn scan_token_stream(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(token) = self.scan_token() {
            tokens.push(token);
        }
        tokens
    }

    pub fn scan_token(&mut self) -> Option<Token> {
        if self.stop {
            return None;
        }

        self.skip_whitespace();

        self.start = self.current;

        let Some(c) = self.consume() else {
            self.stop = true;
            return self.create_token(TokenType::EOF);
        };

        match c {
            // Single-character tokens
            '(' => self.create_token(TokenType::LeftParen),
            ')' => self.create_token(TokenType::RightParen),
            '{' => self.create_token(TokenType::LeftBrace),
            '}' => self.create_token(TokenType::RightBrace),
            '[' => self.create_token(TokenType::LeftBracket),
            ']' => self.create_token(TokenType::RightBracket),
            ',' => self.create_token(TokenType::Comma),
            '.' => self.create_token(TokenType::Dot),
            '-' => self.create_token(TokenType::Minus),
            '+' => self.create_token(TokenType::Plus),
            ';' => self.create_token(TokenType::Semicolon),
            ':' => self.create_token(TokenType::Colon),
            '/' => self.create_token(TokenType::Slash),
            '*' => self.create_token(TokenType::Star),

            // One or more character tokens
            '!' => self.create_matched_token('=', TokenType::BangEqual, TokenType::Bang),
            '=' => self.create_matched_token('=', TokenType::EqualEqual, TokenType::Equal),
            '>' => self.create_matched_token('=', TokenType::GreaterEqual, TokenType::Greater),
            '<' => self.create_matched_token('=', TokenType::LessEqual, TokenType::Less),

            // Literals
            '"' => self.string_literal(),
            '\'' => self.char_literal(),
            '0'..='9' => self.number_literal(),

            // Error
            _ => self.create_token(TokenType::Err(Error::UnrecognizedCharacter(c))),
        }
    }

    fn create_token(&self, token_type: TokenType) -> Option<Token> {
        let lexeme = self.source[self.start..self.current].iter().collect::<String>();
        Some(Token::new(token_type, lexeme, self.line, self.column))
    }

    fn consume(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.current += 1;
        self.column += 1;
        Some(c)
    }

    fn peek(&self) -> Option<char> {
        self.source.get(self.current).copied()
    }

    fn peek_next(&self) -> Option<char> {
        self.source.get(self.current + 1).copied()
    }

    fn create_matched_token(&mut self, c: char, true_token: TokenType, false_token: TokenType) -> Option<Token> {
        if self.peek() == Some(c) {
            self.consume();
            self.create_token(true_token)
        } else {
            self.create_token(false_token)
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            let Some(c) = self.peek() else { return };
            match c {
                ' ' | '\t' | '\r' => {
                    self.consume();
                }
                '\n' => {
                    self.consume();
                    self.line += 1;
                    self.column = 0;
                }
                '/' => {
                    let Some(next_c) = self.peek_next() else { return };
                    if next_c == '/' {
                        self.consume();
                        self.consume();
                        while let Some(c) = self.peek() {
                            self.consume();
                            if c == '\n' {
                                self.line += 1;
                                self.column = 0;
                                break;
                            }
                        }
                    } else if next_c == '*' {
                        self.consume();
                        self.consume();
                        while let Some(c) = self.peek() {
                            self.consume();
                            if c == '\n' {
                                self.line += 1;
                                self.column = 0;
                            } else if c == '*' && self.peek_next() == Some('/') {
                                self.consume();
                                break;
                            }
                        }
                    } else {
                        return;
                    }
                }
                _ => return,
            }
        }
    }

    fn string_literal(&mut self) -> Option<Token> {
        self.consume(); // Consume initial '"'
        let mut string = String::new();
        while let Some(c) = self.consume() {
            if c == '"' {
                return match unescape(&string) {
                    Ok(unescaped) => self.create_token(TokenType::String(unescaped)),
                    Err(e) => self.create_token(TokenType::Err(Error::EscapeError(e))),
                };
            }
            string.push(c);
        }

        self.create_token(TokenType::Err(Error::UnterminatedString))
    }

    fn char_literal(&mut self) -> Option<Token> {
        self.consume(); // Consume initial '\''
        let mut char_string = String::new();
        while let Some(c) = self.consume() {
            if c == '\'' {
                return match unescape(&char_string) {
                    Ok(unescaped) => {
                        if unescaped.len() != 1 {
                            return self.create_token(TokenType::Err(Error::NotSingleCharacter(
                                char_string,
                                unescaped.len(),
                            )));
                        }
                        let c = unescaped.chars().next().expect("Must only be one character");
                        self.create_token(TokenType::Character(c))
                    }
                    Err(e) => self.create_token(TokenType::Err(Error::EscapeError(e))),
                };
            }
            char_string.push(c);
        }

        self.create_token(TokenType::Err(Error::UnterminatedCharacter))
    }

    fn number_literal(&mut self) -> Option<Token> {
        self.consume(); // Consume initial '0'
        let mut number_string = String::new();
        while let Some(c) = self.consume() {
            if c.is_ascii_digit() {
                number_string.push(c);
            } else {
                return self.create_token(TokenType::Integer(number_string.parse().unwrap()));
            }
        }

        self.create_token(TokenType::Err(Error::UnterminatedNumber))
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.scan_token()
    }
}
