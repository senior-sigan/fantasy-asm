use crate::instructions::Instruction::MOV;
use crate::machine::ConstOperand::Literal;
use crate::machine::Machine;
use crate::machine::MutOperand::MutRegister;
use crate::machine::Register::R0;

mod instructions;
mod machine;

fn main() {
    let mut machine = Machine::new();
    machine.append(MOV(MutRegister(R0), Literal(42))); // mov r0 42
    machine.run();

    println!("R0={}", machine.get(R0));
}
