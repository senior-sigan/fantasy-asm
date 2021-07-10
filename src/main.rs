use crate::instructions::Instruction::MOV;
use crate::machine::{Machine, run};
use crate::machine::ConstOperand::Literal;
use crate::machine::MutOperand::MutRegister;
use crate::machine::Register::R0;

mod instructions;
mod machine;

fn main() {
    let mut mem = Machine::new();
    mem.append(MOV(MutRegister(R0), Literal(42))); // mov r0 42
    run(&mut mem);

    println!("R0={}", mem.get(R0));
}
