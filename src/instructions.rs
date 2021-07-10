use crate::instructions::Instruction::*;
use crate::machine::ConstOperand;
use crate::machine::Memory;
use crate::machine::MutOperand;
use crate::machine::Register::CPSR;

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
pub enum Instruction {
    MOV(MutOperand, ConstOperand),
    ADD(MutOperand, ConstOperand),
    MUL(MutOperand, ConstOperand),
    CMP(MutOperand, ConstOperand),
}

pub fn exec(instruction: Instruction, mem: &mut Memory) {
    match instruction {
        ADD(op1, op2) => apply_add(mem, op1, op2),
        MOV(op1, op2) => apply_mov(mem, op1, op2),
        MUL(op1, op2) => apply_mul(mem, op1, op2),
        CMP(op1, op2) => apply_cmp(mem, op1, op2),
    };
}

fn apply_mov(mem: &mut Memory, op1: MutOperand, op2: ConstOperand) {
    mem.set_op(op1, mem.get_op(op2));
}

fn apply_add(mem: &mut Memory, op1: MutOperand, op2: ConstOperand) {
    mem.set_op(op1, mem.get_op(op1.as_const()) + mem.get_op(op2));
}

fn apply_mul(mem: &mut Memory, op1: MutOperand, op2: ConstOperand) {
    mem.set_op(op1, mem.get_op(op1.as_const()) * mem.get_op(op2));
}

fn apply_cmp(mem: &mut Memory, op1: MutOperand, op2: ConstOperand) {
    let lhs = mem.get_op(op1.as_const());
    let rhs = mem.get_op(op2);
    let ord = lhs.cmp(&rhs);
    mem.set_reg(CPSR, ord as i32);
}

#[cfg(test)]
mod tests {
    use crate::machine::{Machine, run};
    use crate::machine::ConstOperand::*;
    use crate::machine::MutOperand::MutRegister;
    use crate::machine::Register::*;

    use super::*;

    #[test]
    fn test_mov() {
        let mut mem = Memory::new();
        let instruction = MOV(MutRegister(R0), Literal(42));

        assert_eq!(mem.get_reg(R0), 0);
        exec(instruction, &mut mem);
        assert_eq!(mem.get_reg(R0), 42);
    }

    #[test]
    fn test_add_literal() {
        let mut mem = Memory::new();
        let instruction = ADD(MutRegister(R0), Literal(41));
        mem.set_reg(R0, 1);
        exec(instruction, &mut mem);
        assert_eq!(mem.get_reg(R0), 42);

        let instruction = ADD(MutRegister(R0), ConstRegister(R1));
        mem.set_reg(R0, 1);
        mem.set_reg(R1, 3);
        exec(instruction, &mut mem);
        assert_eq!(mem.get_reg(R0), 1 + 3);
    }

    #[test]
    fn test_mul() {
        let mut mem = Memory::new();
        let instruction = MUL(MutRegister(R0), ConstRegister(R1));

        mem.set_reg(R0, 3);
        mem.set_reg(R1, 5);
        exec(instruction, &mut mem);
        assert_eq!(mem.get_reg(R0), 3 * 5);

        let instruction = MUL(MutRegister(R0), Literal(42));
        mem.set_reg(R0, 5);
        exec(instruction, &mut mem);
        assert_eq!(mem.get_reg(R0), 5 * 42);
    }

    #[test]
    fn test_cmp_reg() {
        let mut mem = Memory::new();
        let instruction = CMP(MutRegister(R0), ConstRegister(R1));

        mem.set_reg(R0, 10);
        mem.set_reg(R1, 3);
        mem.set_reg(CPSR, 42);
        exec(instruction, &mut mem);
        assert_eq!(mem.get_reg(CPSR), 1);

        mem.set_reg(R0, 3);
        mem.set_reg(R1, 10);
        mem.set_reg(CPSR, 42);
        exec(instruction, &mut mem);
        assert_eq!(mem.get_reg(CPSR), -1);

        mem.set_reg(R0, 5);
        mem.set_reg(R1, 5);
        mem.set_reg(CPSR, 42);
        exec(instruction, &mut mem);
        assert_eq!(mem.get_reg(CPSR), 0);
    }
}