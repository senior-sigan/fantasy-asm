use std::collections::HashMap;

use crate::instructions::{exec, Instruction};
use crate::machine::ConstOperand::*;
use crate::machine::MutOperand::*;
use crate::machine::Register::*;

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
pub enum Register {
    // General purposes register
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    // static base
    SB,
    // stack limit
    SL,
    // intra-procedure-call scratch register
    IP,
    // stack pointer
    SP,
    // link register
    LR,
    // program counter. It is incremented for each instruction.
    PC,
    // current program status register 1 if >, 0 if =, -1 if <
    CPSR,
}


#[derive(Hash, Eq, PartialEq, Copy, Clone)]
pub enum ConstOperand {
    ConstRegister(Register),
    Literal(i32),
    ConstAddress(usize),
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
pub enum MutOperand {
    MutRegister(Register),
    MutAddress(usize),
}

impl From<Register> for ConstOperand {
    fn from(register: Register) -> Self {
        ConstRegister(register)
    }
}

impl From<i32> for ConstOperand {
    fn from(value: i32) -> Self {
        Literal(value)
    }
}

impl From<usize> for ConstOperand {
    fn from(value: usize) -> Self {
        ConstAddress(value)
    }
}

impl From<Register> for MutOperand {
    fn from(register: Register) -> Self {
        MutRegister(register)
    }
}

impl From<usize> for MutOperand {
    fn from(value: usize) -> Self {
        MutAddress(value)
    }
}

impl MutOperand {
    pub(crate) fn as_const(self) -> ConstOperand {
        match self {
            MutRegister(reg) => ConstRegister(reg),
            MutAddress(addr) => ConstAddress(addr),
        }
    }
}

pub struct Memory {
    registers: HashMap<Register, i32>,
    stack: Vec<i32>,
    heap: Vec<i32>,
}

impl Memory {
    pub fn get_op(&self, operand: ConstOperand) -> i32 {
        match operand {
            ConstRegister(reg) => *self.registers.get(&reg).unwrap(),
            Literal(value) => value,
            ConstAddress(_) => todo!("Implement heap read"),
        }
    }

    pub fn set_op(&mut self, operand: MutOperand, value: i32) {
        match operand {
            MutRegister(reg) => self.registers.insert(reg, value),
            MutAddress(_) => todo!("Implement heap write"),
        };
    }

    pub fn get_reg(&self, reg: Register) -> i32 {
        *self.registers.get(&reg).unwrap()
    }

    pub fn set_reg(&mut self, reg: Register, value: i32) {
        self.registers.insert(reg, value);
    }

    pub fn stack_put(&mut self, pointer: i32, value: i32) {
        self.stack[pointer as usize] = value;
    }

    pub fn stack_load(&mut self, pointer: i32) -> i32 {
        self.stack[pointer as usize]
    }

    pub fn new() -> Memory {
        const STACK_SIZE: usize = 1024;
        const HEAP_SIZE: usize = 4096;
        Memory {
            registers: [
                (R0, 0),
                (R1, 0),
                (R2, 0),
                (R3, 0),
                (R4, 0),
                (R5, 0),
                (R6, 0),
                (R7, 0),
                (R8, 0),
                (R8, 0),
                (SB, 0),
                (SL, STACK_SIZE as i32),
                (IP, 0),
                (SP, -1),
                (LR, 0),
                (PC, 0),
                (CPSR, 0),
            ]
                .iter()
                .cloned()
                .collect(),
            stack: vec![0; STACK_SIZE],
            heap: vec![0; HEAP_SIZE],
        }
    }
}

pub struct Machine {
    memory: Memory,
    instructions: Vec<Instruction>,
}

impl Machine {
    pub fn new() -> Machine {
        Machine {
            instructions: Vec::new(),
            memory: Memory::new(),
        }
    }

    pub fn get(&self, register: Register) -> i32 {
        *self.memory.registers.get(&register).unwrap()
    }
    pub fn set(&mut self, register: Register, value: i32) {
        self.memory.registers.insert(register, value);
    }
    pub fn append(&mut self, instruction: Instruction) {
        self.instructions.push(instruction)
    }
    pub fn append_all(&mut self, instructions: Vec<Instruction>) {
        self.instructions.extend(instructions);
    }

    pub fn run(&mut self) {
        while self.step() {}
    }

    pub fn step(&mut self) -> bool {
        let instruction = self.instructions.get(self.get(PC) as usize);
        if instruction.is_none() {
            return false;
        }
        exec(*instruction.unwrap(), &mut self.memory);
        self.set(PC, self.get(PC) + 1);
        return true;
    }
}

#[cfg(test)]
mod tests {
    use crate::instructions::Instruction::{ADD, CMP, JE, JG, MOV, MUL};

    use super::*;

    #[test]
    fn test_fib() {
        let mut machine = Machine::new();
        machine.append_all(Vec::from([
            MOV(R0.into(), 5.into()),
            MOV(R1.into(), 1.into()),
            MUL(R1.into(), R0.into()),
            ADD(R0.into(), Literal(-1)),
            CMP(R0.into(), 0.into()),
            JG(Literal(-4)),
        ]));

        machine.run();

        assert_eq!(machine.get(R0), 0);
        assert_eq!(machine.get(R1), 1 * 2 * 3 * 4 * 5);
    }
}