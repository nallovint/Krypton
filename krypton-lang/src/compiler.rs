use crate::bytecode;
use crate::bytecode::Klass;
use crate::compiler::lex::Lexer;
use crate::compiler::parse::Parser;

pub mod lex;
pub mod parse;

type Result<T> = parse::Result<T>;

#[allow(clippy::missing_errors_doc)]
pub fn compile(input: &str) -> Result<Klass> {
    debug!("Tokens: {}", Lexer::new(input).scan_token_stream());
    let lexer = Lexer::new(input);
    let parser = Parser::from(lexer);
    let klass = parser.parse()?;
    debug!("{}", bytecode::dump(&klass));
    Ok(klass)
}
