use krypton_lang::assembly;
use krypton_lang::bytecode::*;
use krypton_vm::v1::VM;

#[cfg(not(release))]
fn init_logger() {
    simple_log::quick().unwrap();
    if !simple_log::is_release() {
        let config = simple_log::LogConfigBuilder::builder()
            .time_format("")
            .level(simple_log::log_level::DEBUG)
            .output_console()
            .build();
        simple_log::update_log_conf(config).unwrap();
        println!("Debug mode");
    } else {
        println!("Release mode");
        simple_log::update_log_level(simple_log::log_level::INFO).unwrap();
    }
}

fn main() {
    init_logger();

    let mut bytecode = KlassBytecode::new("Test".to_string(), Version::latest());
    println!("{}", bytecode);

    bytecode.push_constant(ConstantPoolEntry::Double(1.2));
    bytecode.push_constant(ConstantPoolEntry::Int(223));

    bytecode.push_instruction(Instruction::LoadConstant8 { cp_addr: 0 });
    bytecode.push_instruction(Instruction::LoadConstant16 { cp_addr: 1 });
    bytecode.push_instruction(Instruction::Return);

    bytecode.push_line_number(LineNumberEntry::new(1, 1));

    println!("{}\n", bytecode);

    let bytes = assembly::klass_to_bytes(&bytecode);
    match bytes {
        Ok(bytes) => {
            print_bytes(&bytes);

            println!("Starting VM...");
            let now = std::time::Instant::now();
            let mut vm = VM::new();
            let elapsed = now.elapsed();
            println!(
                "VM started in {}s {}ms {}ns",
                elapsed.as_secs(),
                elapsed.subsec_millis(),
                elapsed.subsec_nanos() % 1_000_000
            );
            let now = std::time::Instant::now();
            match vm.load_and_run(bytes) {
                Ok(_) => {
                    let elapsed = now.elapsed();
                    println!(
                        "VM finished after {}s {}ms {}ns",
                        elapsed.as_secs(),
                        elapsed.subsec_millis(),
                        elapsed.subsec_nanos() % 1_000_000
                    );
                }
                Err(error) => println!("{}", error),
            }
        }
        Err(error) => println!("{}", error),
    }
}

fn print_bytes(bytes: &[u8]) {
    println!("|  Binary  | Hex | Dec | Char |");
    println!("{}", "-".repeat(31));
    for byte in bytes {
        print_byte(*byte);
    }
    println!()
}

fn print_byte(byte: u8) {
    println!(
        "| {:08b} | {:02x}  | {:03} | '{}'  |",
        byte,
        byte,
        byte,
        match byte {
            b' ' => ' ',
            _ if (32..=126).contains(&byte) => byte as char,
            _ => '�',
        }
    );
}
