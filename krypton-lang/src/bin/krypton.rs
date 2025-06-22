use std::env;
use std::fs;
use std::process;

use krypton_lang::compiler::lex::Lexer;
use krypton_lang::compiler::parse::Parser;
use krypton_lang::vm::VM;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        process::exit(1);
    }

    let source = match fs::read_to_string(&args[1]) {
        Ok(source) => source,
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            process::exit(1);
        }
    };

    let lexer = Lexer::new(&source);
    let tokens = lexer.scan_token_stream();
    let parser = Parser::from(tokens);
    let klass = match parser.parse() {
        Ok(klass) => klass,
        Err(errors) => {
            for error in errors {
                eprintln!("Error: {:?}", error);
            }
            process::exit(1);
        }
    };

    let mut vm = VM::new();
    if let Err(error) = vm.load_and_run(&klass) {
        eprintln!("Runtime error: {}", error);
        process::exit(1);
    }
} 