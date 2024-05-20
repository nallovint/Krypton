use krypton_lang::bytecode::{BinaryDisplayExt, encode, Identifier, Instruction, Klass, LineNumberEntry, Value};
use krypton_lang::debug;
use krypton_lang::vm::VM;

fn main() {
    let klass = Klass {
        name: Some(Identifier::new("HelloWorld").unwrap()),
        version: 1,

        constant_pool: vec![Value::Double(1.2), Value::Double(3.4), Value::Double(5.6)],

        instructions: vec![
            Instruction::LoadConstant { cp_addr: 0 },
            Instruction::LoadConstant { cp_addr: 1 },
            Instruction::Add,
            Instruction::LoadConstant { cp_addr: 2 },
            Instruction::Divide,
            Instruction::Negate,
            Instruction::Return,
        ],

        line_number_table: Some(vec![LineNumberEntry {
            start_pc: 0,
            line_number: 1,
        }]),
    };

    let bytes = encode(&klass);
    debug!("\n{}", bytes.display_binary());

    println!("Starting VM...");
    let start = std::time::Instant::now();
    let mut vm = VM::new();
    println!("VM started in {}", formatted_elapsed(start));

    println!("Loading bytecode...");
    let start = std::time::Instant::now();
    vm.load(&bytes).unwrap();
    println!("VM loaded bytecode after {}", formatted_elapsed(start));

    println!("Running VM...");
    let start = std::time::Instant::now();
    vm.run().unwrap();
    println!("VM finished after {}", formatted_elapsed(start));
}

fn formatted_elapsed(start: std::time::Instant) -> String {
    let elapsed = start.elapsed();
    format!(
        "{}s {}ms {}ns",
        elapsed.as_secs(),
        elapsed.subsec_millis(),
        elapsed.subsec_nanos() % 1_000_000
    )
}
