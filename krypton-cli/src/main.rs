use std::io::{Read, Write};
use std::path::PathBuf;
use std::process::exit;

use clap::{arg, ArgAction, ArgGroup, command};

fn main() {
    let matches = command!()
        .arg(
            arg!(--repl "Starts a REPL and enters it")
                .help("REPL allows you to execute code interactively")
                .action(ArgAction::SetTrue),
        )
        .arg(
            arg!(-f --file <KRYPTON_FILE> "*.kr source file to compile and run")
                .help("The Krypton source file (.kr) to compile and run")
                .value_parser(file_is_valid),
        )
        .group(ArgGroup::new("mode").required(true).args(["file", "repl"]))
        .get_matches();

    if matches.contains_id("file") {
        let input = matches.get_one::<String>("file").expect("Main: No file specified");
        run_file(input);
    } else if matches.contains_id("repl") {
        repl();
    }
}

fn repl() {
    println!("Welcome to the Krypton REPL!");
    loop {
        print!("> ");
        let mut input = String::new();
        std::io::stdout().flush().expect("REPL: Could not flush stdout");
        std::io::stdin()
            .read_line(&mut input)
            .expect("REPL: Could not read from stdin");

        if input.starts_with("exit") || input.starts_with("quit") || input.starts_with(":q") {
            break;
        }

        if input.trim().is_empty() {
            let result = interpret(&input);
            match result {
                Ok(result) => println!("{result}"),
                Err(error) => exit(error),
            }
        }
    }
}

fn run_file(source: &str) {
    println!("Running Krypton file");
    let result = interpret(source);
    match result {
        Ok(result) => println!("{result}"),
        Err(error) => exit(error),
    }
}

fn file_is_valid(file: &str) -> Result<String, String> {
    let path = PathBuf::from(file);
    if let Some(ext) = path.extension() {
        if ext != "kr" {
            return Err(format!(
                "File has an invalid extension, expected `.kr`, got `.{}`",
                ext.to_string_lossy()
            ));
        }
    } else {
        return Err("File has no extension, expected `*.kr`".to_owned());
    }

    let file = std::fs::File::open(path);
    let file = file.map_err(|e| format!("{e}"))?;
    let mut file = std::io::BufReader::new(file);
    let mut contents = String::new();
    file.read_to_string(&mut contents).map_err(|e| format!("{e}"))?;
    Ok(contents)
}

fn interpret(_source: &str) -> Result<String, i32> {
    // Ok(source.to_string())
    todo!()
}
