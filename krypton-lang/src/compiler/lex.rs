use std::fmt::Display;
use std::ops::Deref;

use unescaper::unescape;

#[derive(Debug)]
pub enum Error {
    UnrecognizedCharacter(char),
    UnterminatedString,
    UnterminatedCharacter,
    EscapeError(unescaper::Error),
    NotSingleCharacter(String, usize),
}

impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (Error::UnrecognizedCharacter(_), Error::UnrecognizedCharacter(_))
                | (Error::UnterminatedString, Error::UnterminatedString)
                | (Error::UnterminatedCharacter, Error::UnterminatedCharacter)
                | (Error::EscapeError(_), Error::EscapeError(_))
                | (Error::NotSingleCharacter(..), Error::NotSingleCharacter(..))
        )
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Error::UnrecognizedCharacter(c) => format!("Unrecognized character '{}'", c),
            Error::UnterminatedString => "Unterminated string".to_string(),
            Error::UnterminatedCharacter => "Unterminated character".to_string(),
            Error::EscapeError(e) => format!("Escape error: {}", e),
            Error::NotSingleCharacter(s, n) => format!(
                "Single quotes must contain only 1 character, '{}' has {} characters",
                s, n
            ),
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct TokenStream(Vec<Token>);

impl IntoIterator for TokenStream {
    type Item = Token;
    type IntoIter = std::vec::IntoIter<Token>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl From<Vec<Token>> for TokenStream {
    fn from(tokens: Vec<Token>) -> Self {
        Self(tokens)
    }
}

impl Deref for TokenStream {
    type Target = Vec<Token>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for TokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{\n{}\n}}",
            self.0
                .iter()
                .map(|token| format!("  {}", token))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

#[derive(Debug, PartialEq)]
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

    // One or more character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier(String),
    String(String),
    Character(char),
    Integer(i64),
    Float(f64),

    // Keywords
    /// [Add keywords to here too!](Lexer::create_identifier_token)
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

    // End of token stream
    Eof,
    Err(Error),
}

impl TokenType {
    pub fn to_enum_string(&self) -> &'static str {
        match self {
            TokenType::LeftParen => "LeftParen",
            TokenType::RightParen => "RightParen",
            TokenType::LeftBrace => "LeftBrace",
            TokenType::RightBrace => "RightBrace",
            TokenType::LeftBracket => "LeftBracket",
            TokenType::RightBracket => "RightBracket",
            TokenType::Comma => "Comma",
            TokenType::Dot => "Dot",
            TokenType::Minus => "Minus",
            TokenType::Plus => "Plus",
            TokenType::Semicolon => "Semicolon",
            TokenType::Colon => "Colon",
            TokenType::Slash => "Slash",
            TokenType::Star => "Star",
            TokenType::Bang => "Bang",
            TokenType::BangEqual => "BangEqual",
            TokenType::Equal => "Equal",
            TokenType::EqualEqual => "EqualEqual",
            TokenType::Greater => "Greater",
            TokenType::GreaterEqual => "GreaterEqual",
            TokenType::Less => "Less",
            TokenType::LessEqual => "LessEqual",
            TokenType::Identifier(_) => "Identifier",
            TokenType::String(_) => "String",
            TokenType::Character(_) => "Character",
            TokenType::Integer(_) => "Integer",
            TokenType::Float(_) => "Float",
            TokenType::Class => "Class",
            TokenType::If => "If",
            TokenType::Else => "Else",
            TokenType::True => "True",
            TokenType::False => "False",
            TokenType::While => "While",
            TokenType::For => "For",
            TokenType::Fn => "Fn",
            TokenType::Null => "Null",
            TokenType::Print => "Print",
            TokenType::Return => "Return",
            TokenType::Super => "Super",
            TokenType::This => "This",
            TokenType::Var => "Var",
            TokenType::Eof => "EOF",
            TokenType::Err(_) => "Err",
        }
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            TokenType::Err(e) => e.to_string(),
            TokenType::Identifier(id) => id.to_string(),
            TokenType::String(s) => s.to_string(),
            TokenType::Character(c) => c.to_string(),
            TokenType::Integer(i) => i.to_string(),
            TokenType::Float(f) => f.to_string(),
            _ => self.to_enum_string().to_string(),
        })
    }
}

#[derive(Debug, PartialEq)]
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

    pub fn dummy() -> Self {
        Self {
            token_type: TokenType::Eof,
            lexeme: "".to_string(),
            line: 0,
            column: 0,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{:02} {:<12} {:<25} # {}",
            self.line,
            self.column,
            format!("{}: ", self.token_type.to_enum_string()),
            format!("'{}'", self.lexeme),
            self.token_type
        )
    }
}

#[derive(Debug)]
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

    pub fn scan_token_stream(mut self) -> TokenStream {
        let mut tokens = Vec::new();
        while let Some(token) = self.scan_token() {
            tokens.push(token);
        }
        TokenStream(tokens)
    }

    pub fn scan_token(&mut self) -> Option<Token> {
        if self.stop {
            return None;
        }

        self.skip_whitespace();

        debug!("Skipped whitespace");

        self.start = self.current;

        debug!("Start: {:?}", self.start);
        debug!("Current: {:?}", self.current);
        debug!("Peek: {:?}", self.peek());

        let Some(c) = self.consume() else {
            self.stop = true;
            return self.create_token(TokenType::Eof);
        };

        debug!("C: {c}");

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
            '0'..='9' => self.number_literal(c),

            _ => {
                if is_identifier_start(c) {
                    self.identifier(c)
                } else {
                    self.create_error_token(Error::UnrecognizedCharacter(c))
                }
            }
        }
    }

    fn create_token(&self, token_type: TokenType) -> Option<Token> {
        let lexeme = self.source[self.start..self.current].iter().collect::<String>();
        Some(Token::new(token_type, lexeme, self.line, self.column))
    }

    fn consume(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.current += 1;
        if c == '\n' {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }
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
                ' ' | '\t' | '\r' | '\n' => {
                    debug!("Skipping whitespace, C: {c}");
                    self.consume();
                }
                '/' => {
                    debug!("Skipping whitespace, C: {c}");
                    let Some(next_c) = self.peek_next() else { return };
                    if next_c == '/' {
                        debug!("Single line comment Next Peek: {:?}", next_c);
                        self.consume(); // First '/'
                        self.consume(); // Second '/'
                        'inner: while let Some(c) = self.peek() {
                            debug!("Multiline comment Peek: {:?}", c);
                            self.consume();
                            if c == '\n' {
                                break 'inner;
                            }
                        }
                    } else if next_c == '*' {
                        debug!("Multiline comment Next Peek: {:?}", next_c);
                        self.consume(); // '/'
                        self.consume(); // '*'
                        'inner: while let Some(c) = self.peek() {
                            debug!("Multiline comment Peek: {:?}", c);
                            self.consume();
                            if c == '*' && self.peek() == Some('/') {
                                self.consume(); // Consumes '/'
                                break 'inner;
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
        let mut string = String::new();
        let mut escape_next = false;
        while let Some(c) = self.consume() {
            if c != '"' || escape_next {
                // This is a bit of a hack to allow `"` to be escaped
                // if it has a `\` before it
                // The !escape_next check makes sure that if char is a backslash
                // And the previous char is also a backslash
                // Then the next char should not be escaped
                // "\\" - Without it, the last `"` would be escaped, and it'll cause
                // an error
                escape_next = !escape_next && c == '\\';
                string.push(c);
                continue;
            }
            return match unescape(&string) {
                Ok(unescaped) => self.create_token(TokenType::String(unescaped)),
                Err(e) => self.create_error_token(Error::EscapeError(e)),
            };
        }

        self.create_error_token(Error::UnterminatedString)
    }

    fn char_literal(&mut self) -> Option<Token> {
        let mut char_string = String::new();
        let mut escape_next = false;
        while let Some(c) = self.consume() {
            if c != '\'' || escape_next {
                // This is a bit of a hack to allow `'` to be escaped
                // if it has a `\` before it
                // The !escape_next check makes sure that if char is a backslash
                // And the previous char is also a backslash
                // Then the next char should not be escaped
                // '\\' - Without it, the last `'` would be escaped, and it'll cause
                // an error
                escape_next = !escape_next && c == '\\';
                char_string.push(c);
                continue;
            }
            debug!("Char literal: |{}|", char_string);
            return match unescape(&char_string) {
                Ok(unescaped) => {
                    debug!("Unescaped: |{}|", unescaped);
                    // unescaped.len() return amount of bytes, not chars,
                    // so we need to use chars().count()
                    if unescaped.chars().count() != 1 {
                        return self.create_error_token(Error::NotSingleCharacter(char_string, unescaped.len()));
                    }
                    let c = unescaped.chars().next().expect("Must only be one character");
                    self.create_token(TokenType::Character(c))
                }
                Err(e) => self.create_error_token(Error::EscapeError(e)),
            };
        }

        self.create_error_token(Error::UnterminatedCharacter)
    }

    fn number_literal(&mut self, initial_digit: char) -> Option<Token> {
        // TODO: Use library "lexical" to parse numbers to allow for rust-like
        // syntax for floats and integers
        let mut number_string = String::new();
        number_string.push(initial_digit);
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() || c == '.' {
                self.consume();
                number_string.push(c);
                continue;
            }
            return self.create_number_token(number_string);
        }

        self.create_number_token(number_string)
    }

    fn create_number_token(&self, number_string: String) -> Option<Token> {
        let float = number_string.parse::<f64>();
        let int = number_string.parse::<i64>();

        match (float, int) {
            (Ok(f), Err(_)) => self.create_token(TokenType::Float(f)),
            (_, Ok(i)) => self.create_token(TokenType::Integer(i)),
            _ => panic!("Unexpected number: {}", number_string),
        }
    }

    fn identifier(&mut self, start: char) -> Option<Token> {
        let mut identifier = String::new();
        identifier.push(start);
        while let Some(c) = self.peek() {
            if is_identifier_continue(c) {
                self.consume(); // Consume the character
                identifier.push(c);
            } else {
                return self.create_identifier_token(&identifier);
            }
        }
        self.create_token(TokenType::Identifier(identifier))
    }

    fn create_identifier_token(&self, identifier: &str) -> Option<Token> {
        match identifier {
            "class" => self.create_token(TokenType::Class),
            "if" => self.create_token(TokenType::If),
            "else" => self.create_token(TokenType::Else),
            "true" => self.create_token(TokenType::True),
            "false" => self.create_token(TokenType::False),
            "while" => self.create_token(TokenType::While),
            "for" => self.create_token(TokenType::For),
            "fn" => self.create_token(TokenType::Fn),
            "null" => self.create_token(TokenType::Null),
            "print" => self.create_token(TokenType::Print),
            "return" => self.create_token(TokenType::Return),
            "super" => self.create_token(TokenType::Super),
            "this" => self.create_token(TokenType::This),
            "var" => self.create_token(TokenType::Var),
            _ => self.create_token(TokenType::Identifier(identifier.to_string())),
        }
    }

    fn create_error_token(&mut self, error: Error) -> Option<Token> {
        self.create_token(TokenType::Err(error))
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.scan_token()
    }
}

pub fn is_identifier_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

pub fn is_identifier_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_' || c == '$'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_identifier_start() {
        assert!(is_identifier_start('a'));
        assert!(is_identifier_start('G'));
        assert!(is_identifier_start('z'));
        assert!(is_identifier_start('_'));

        assert!(!is_identifier_start('$'));
        assert!(!is_identifier_start(' '));
        assert!(!is_identifier_start('0'));
        assert!(!is_identifier_start('-'));
        assert!(!is_identifier_start('!'));
        assert!(!is_identifier_start('@'));
        assert!(!is_identifier_start('\0'));
    }

    #[test]
    fn test_is_identifier_continue() {
        assert!(is_identifier_continue('a'));
        assert!(is_identifier_continue('G'));
        assert!(is_identifier_continue('z'));
        assert!(is_identifier_continue('_'));
        assert!(is_identifier_continue('$'));
        assert!(is_identifier_continue('0'));
        assert!(is_identifier_continue('5'));
        assert!(is_identifier_continue('9'));

        assert!(!is_identifier_continue(' '));
        assert!(!is_identifier_continue('-'));
        assert!(!is_identifier_continue('!'));
        assert!(!is_identifier_continue('@'));
        assert!(!is_identifier_continue('\0'));
    }

    macro_rules! lexing_tests {
        ($($name:ident: $value:expr,)*) => {$(
            #[test]
            fn $name() {
                let (input, expected) = $value;
                let tokens = Lexer::new(input).scan_token_stream();
                println!("{}", tokens);

                for (i, t) in tokens.iter().enumerate() {
                    assert_eq!(t.token_type, expected[i]);
                }
            }
        )*}
    }

    lexing_tests! {
        test_basic_case: ("a", vec![
            TokenType::Identifier("a".to_string()),
            TokenType::Eof,
        ]),

        test_simple_example: (r#"class Hello { fn sayHello() { print("Hello\n\tWorld!" + 11.7 / 2); } }"#, vec![
            TokenType::Class,
            TokenType::Identifier("Hello".to_string()),
            TokenType::LeftBrace,
            TokenType::Fn,
            TokenType::Identifier("sayHello".to_string()),
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::LeftBrace,
            TokenType::Print,
            TokenType::LeftParen,
            TokenType::String("Hello\n\tWorld!".to_string()),
            TokenType::Plus,
            TokenType::Float(11.7),
            TokenType::Slash,
            TokenType::Integer(2),
            TokenType::RightParen,
            TokenType::Semicolon,
            TokenType::RightBrace,
            TokenType::RightBrace,
            TokenType::Eof,
        ]),

        test_unicode_chars: (r#"class Hello { fn sayHello() {שלום print("Hello\n\tWorld!" + 11.7 / 2); } }"#, vec![
            TokenType::Class,
            TokenType::Identifier("Hello".to_string()),
            TokenType::LeftBrace,
            TokenType::Fn,
            TokenType::Identifier("sayHello".to_string()),
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::LeftBrace,
            TokenType::Err(Error::UnrecognizedCharacter('ש')),
            TokenType::Err(Error::UnrecognizedCharacter('ל')),
            TokenType::Err(Error::UnrecognizedCharacter('ו')),
            TokenType::Err(Error::UnrecognizedCharacter('ם')),
            TokenType::Print,
            TokenType::LeftParen,
            TokenType::String("Hello\n\tWorld!".to_string()),
            TokenType::Plus,
            TokenType::Float(11.7),
            TokenType::Slash,
            TokenType::Integer(2),
            TokenType::RightParen,
            TokenType::Semicolon,
            TokenType::RightBrace,
            TokenType::RightBrace,
            TokenType::Eof,
        ]),

        test_comments_and_char_literals: (
            &r#" // Test%n /'H' '\'' 'ל' '\\' /* Some text %n %n with stuff */ '\0' '\n' '\u{2990}' '\\\\'"#
            .replace("%n", "\n"),
            vec![
                TokenType::Slash,
                TokenType::Character('H'),
                TokenType::Character('\''),
                TokenType::Character('ל'),
                TokenType::Character('\\'),
                TokenType::Character('\0'),
                TokenType::Character('\n'),
                TokenType::Character('\u{2990}'),
                TokenType::Err(Error::NotSingleCharacter(r#"\\"#.to_string(), 2)),
                TokenType::Eof,
            ]
        ),
        test_strings: (r#""Hello\n\tWorld!" "'c'" "ל" "\\" "\"Quote\"" "\\\"" "#, vec![
            TokenType::String("Hello\n\tWorld!".to_string()),
            TokenType::String("'c'".to_string()),
            TokenType::String("ל".to_string()),
            TokenType::String("\\".to_string()),
            TokenType::String("\"Quote\"".to_string()),
            TokenType::String("\\\"".to_string()),
            TokenType::Eof,
        ]),
    }
}
