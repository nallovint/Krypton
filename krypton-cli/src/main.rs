use krypton_lang::bytecode::*;

fn main() {
    println!("Hello, world!");
    let mut bytecode = Bytecode::new();
    println!("{}", bytecode);
    bytecode.push(Instruction::Return);
    bytecode.push(Instruction::Return);
    println!("{}", bytecode);
}
