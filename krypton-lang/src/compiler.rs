use crate::bytecode::Klass;
use crate::compiler::lex::Lexer;
use crate::compiler::parse::Parser;

pub mod lex;
pub mod parse;

type Result<T> = std::result::Result<T, parse::Error>;

pub fn compile(input: &str) -> Result<Klass> {
    let lexer = Lexer::new(input);
    let parser = Parser::from(lexer);
    parser.parse()
}
