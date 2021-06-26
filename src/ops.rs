use std::collections::HashMap;

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
enum Register {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    LINE,
    CMP,
}

struct Memory {
    registers: HashMap<Register, i32>,
    instructions: Vec<Box<dyn Ops>>,
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            instructions: Vec::new(),
            registers: [
                (Register::R0, 0),
                (Register::R1, 0),
                (Register::R2, 0),
                (Register::R3, 0),
                (Register::R4, 0),
                (Register::R5, 0),
                (Register::R6, 0),
                (Register::R7, 0),
                (Register::R8, 0),
                (Register::LINE, 0),
                (Register::CMP, 0),
            ]
                .iter()
                .cloned()
                .collect(),
        }
    }

    pub fn get(&self, register: Register) -> i32 {
        *self.registers.get(&register).unwrap()
    }
    pub fn set(&mut self, register: Register, value: i32) {
        self.registers.insert(register, value);
    }
    pub fn append<T: Ops + 'static>(&mut self, instruction: T) {
        self.instructions.push(Box::new(instruction))
    }
}

trait Value: Copy {
    fn get(&self, mem: &Memory) -> i32;
}

trait MutableValue: Value + Copy {
    fn set(&self, mem: &mut Memory, value: i32);
}

#[derive(Copy, Clone)]
struct ValueLiteral {
    value: i32,
}

impl ValueLiteral {
    pub fn new(value: i32) -> ValueLiteral {
        ValueLiteral { value }
    }
}

impl Value for ValueLiteral {
    fn get(&self, mem: &Memory) -> i32 {
        self.value
    }
}

#[derive(Copy, Clone)]
struct ValueRegister {
    register: Register,
}

impl ValueRegister {
    pub fn new(register: Register) -> ValueRegister {
        ValueRegister { register }
    }
}

impl Value for ValueRegister {
    fn get(&self, mem: &Memory) -> i32 {
        mem.get(self.register)
    }
}

impl MutableValue for ValueRegister {
    fn set(&self, mem: &mut Memory, value: i32) {
        mem.set(self.register, value)
    }
}

trait Ops {
    fn apply(&self, mem: &mut Memory);
}

#[derive(Copy, Clone)]
struct Add<T1: MutableValue, T2: Value> {
    arg1: T1,
    arg2: T2,
}

impl<T1: MutableValue, T2: Value> Add<T1, T2> {
    pub fn new(arg1: T1, arg2: T2) -> Add<T1, T2> {
        Add { arg1, arg2 }
    }
}

impl<T1, T2> Ops for Add<T1, T2>
    where
        T1: MutableValue,
        T2: Value,
{
    fn apply(&self, mem: &mut Memory) {
        self.arg1.set(mem, self.arg1.get(mem) + self.arg2.get(mem))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let mut mem = Memory::new();
        mem.set(Register::R0, 5);
        let op = Add::new(
            ValueRegister::new(Register::R0),
            ValueLiteral::new(42),
        );
        op.apply(&mut mem);
        assert_eq!(mem.get(Register::R0), 42 + 5)
    }
}
