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
    // current program status register
    CPSR,
}


#[derive(Hash, Eq, PartialEq, Copy, Clone)]
pub enum ConstOperand {
    ConstRegister(Register),
    Literal(i32),
    ConstAddress(i32),
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
pub enum MutOperand {
    MutRegister(Register),
    MutAddress(i32),
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
}

impl Memory {
    pub(crate) fn get_op(&self, operand: ConstOperand) -> i32 {
        match operand {
            ConstRegister(reg) => *self.registers.get(&reg).unwrap(),
            Literal(value) => value,
            ConstAddress(_) => todo!("Implement heap read"),
        }
    }

    pub(crate) fn set_op(&mut self, operand: MutOperand, value: i32) {
        match operand {
            MutRegister(reg) => self.registers.insert(reg, value),
            MutAddress(_) => todo!("Implement heap write"),
        };
    }

    pub(crate) fn get_reg(&self, reg: Register) -> i32 {
        *self.registers.get(&reg).unwrap()
    }

    pub(crate) fn set_reg(&mut self, reg: Register, value: i32) {
        self.registers.insert(reg, value);
    }

    pub fn new() -> Memory {
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
                (SL, 0),
                (IP, 0),
                (SP, 0),
                (LR, 0),
                (PC, 0),
                (CPSR, 0),
            ]
                .iter()
                .cloned()
                .collect(),
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
}

pub fn run(machine: &mut Machine) {
    for instruction in machine.instructions.iter() {
        exec(*instruction, &mut machine.memory)
    }
}