use krypton_lang::bytecode::*;

fn main() {
    println!("Hello, world!");
    let mut bytecode = Bytecode::new("Test".to_string(), Version::new(1, 0));
    println!("{}", bytecode);
    bytecode.push_constant(ConstantPoolEntry::Double(1.2));
    bytecode.push_instruction(Instruction::Constant { cp_addr: 0 });
    bytecode.push_instruction(Instruction::Return);
    println!("{}", bytecode);
}
