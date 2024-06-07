use crate::bytecode::Klass;
use crate::compiler::lex::Lexer;
use crate::compiler::parse::Parser;

pub mod lex;
pub mod parse;

type Result<T> = parse::Result<T>;

pub fn compile(input: &str) -> Result<Klass> {
    let lexer = Lexer::new(input);
    let parser = Parser::from(lexer);
    let klass = parser.parse()?;
    debug!("{}", dump(&klass));
    Ok(klass)
}

pub fn dump(klass: &Klass) -> String {
    let mut formatted = String::new();
    formatted.push_str("Instructions:\n");
    for instruction in &klass.instructions {
        formatted.push_str(&format!("{}\n", instruction));
    }
    formatted
}
