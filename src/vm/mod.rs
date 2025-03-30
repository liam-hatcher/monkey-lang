use std::fmt;

use crate::{
    code::{Instructions, Opcode, read_u16},
    compiler::ByteCode,
    object::{Integer, Object, ObjectType, ObjectValue},
};

const STACK_SIZE: usize = 2048;

#[derive(Debug)]
pub enum VMError {
    StackOverflow,
    UnknownOpcode,
    TypeMismatch,
}

impl fmt::Display for VMError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            VMError::StackOverflow => write!(f, "Stack Overflow occurred"),
            VMError::UnknownOpcode => write!(f, "Unknown opcode"),
            VMError::TypeMismatch => write!(f, "invalid type"),
        }
    }
}

impl std::error::Error for VMError {}

pub struct MonkeyVM {
    constants: Vec<Box<dyn Object>>,
    instructions: Instructions,

    stack: Vec<Box<dyn Object>>,
    stack_pointer: usize,
}

impl MonkeyVM {
    pub fn new(bytecode: ByteCode) -> Self {
        Self {
            instructions: bytecode.instructions,
            constants: bytecode.constants,
            stack: Vec::with_capacity(STACK_SIZE),
            stack_pointer: 0,
        }
    }

    pub fn stack_top(&self) -> Option<&Box<dyn Object>> {
        if self.stack_pointer > 0 {
            Some(&self.stack[self.stack_pointer - 1])
        } else {
            None
        }
    }

    fn push(&mut self, obj: Box<dyn Object>) -> Result<(), VMError> {
        if self.stack_pointer >= STACK_SIZE {
            return Err(VMError::StackOverflow);
        }

        self.stack.push(obj.clone_box());
        self.stack_pointer += 1;

        Ok(())
    }

    fn pop(&mut self) -> Box<dyn Object> {
        let obj = self.stack.pop().unwrap();
        self.stack_pointer -= 1;

        obj
    }

    fn unwrap_integer(&self, b: Box<dyn Object>) -> Result<i32, VMError> {
        if b.kind() != ObjectType::Integer {
            return Err(VMError::TypeMismatch);
        }

        match b.get_value() {
            ObjectValue::Int(i) => Ok(i as i32),
            _ => unreachable!(),
        }
    }

    pub fn run(&mut self) -> Result<(), VMError> {
        let mut ip = 0;
        while ip < self.instructions.len() {
            let op = Opcode::from_u8(self.instructions[ip]).unwrap();

            match op {
                Opcode::OpConstant => {
                    let const_index = read_u16(&self.instructions[ip + 1..]);
                    ip += 2;
                    let obj = &self.constants[const_index as usize];

                    self.push(obj.clone())?;
                }
                Opcode::OpAdd => {
                    let right = self.pop();
                    let left = self.pop();
                    let left_value = self.unwrap_integer(left)?;
                    let right_value = self.unwrap_integer(right)?;
                    println!("LEFT {} RIGHT {}", left_value, right_value);
                    let result = left_value + right_value;
                    self.push(Box::new(Integer {
                        value: result as i64,
                    }))?;
                }
                _ => return Err(VMError::UnknownOpcode),
            }

            ip += 1;
        }

        Ok(())
    }
}

#[cfg(test)]
mod vm_tests;
