use std::fmt;

use byteorder::{BigEndian, ByteOrder};

use crate::{
    code::{Instructions, Opcode, read_u16},
    compiler::ByteCode,
    object::{Boolean, Integer, Null, Object, ObjectType, ObjectValue},
};

const STACK_SIZE: usize = 2048;

#[derive(Debug)]
pub enum VMError {
    StackOverflow,
    // UnknownOpcode,
    TypeMismatch,
    InvalidOperator,
    UnknownOperator,
}

impl fmt::Display for VMError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VMError::StackOverflow => write!(f, "Stack Overflow occurred"),
            // VMError::UnknownOpcode => write!(f, "Unknown opcode"),
            VMError::TypeMismatch => write!(f, "invalid type"),
            VMError::InvalidOperator => write!(f, "invalid operator"),
            VMError::UnknownOperator => write!(f, "unknown operator"),
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
            stack: vec![Box::new(Null); STACK_SIZE],
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

    pub fn last_popped_stack_elem(&self) -> &Box<dyn Object> {
        &self.stack[self.stack_pointer]
    }

    fn push(&mut self, obj: Box<dyn Object>) -> Result<(), VMError> {
        if self.stack_pointer >= STACK_SIZE {
            return Err(VMError::StackOverflow);
        }

        self.stack[self.stack_pointer] = obj.clone_box();

        self.stack_pointer += 1;

        Ok(())
    }

    fn pop(&mut self) -> Box<dyn Object> {
        let obj = self.stack[self.stack_pointer - 1].clone();
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

    fn execute_binop(&mut self, op: Opcode) -> Result<(), VMError> {
        let right = self.pop();
        let left = self.pop();
        let left_value = self.unwrap_integer(left)?;
        let right_value = self.unwrap_integer(right)?;

        let result = match op {
            Opcode::OpAdd => left_value + right_value,
            Opcode::OpSub => left_value - right_value,
            Opcode::OpMul => left_value * right_value,
            Opcode::OpDiv => left_value / right_value,
            _ => return Err(VMError::InvalidOperator),
        };

        self.push(Box::new(Integer {
            value: result as i64,
        }))?;

        Ok(())
    }

    fn execute_integer_comparison(
        &mut self,
        op: Opcode,
        left: Box<dyn Object>,
        right: Box<dyn Object>,
    ) -> Result<(), VMError> {
        let left_value = self.unwrap_integer(left)?;
        let right_value = self.unwrap_integer(right)?;

        match op {
            Opcode::OpEqual => self.push(Box::new(Boolean {
                value: right_value == left_value,
            })),
            Opcode::OpNotEqual => self.push(Box::new(Boolean {
                value: right_value != left_value,
            })),
            Opcode::OpGreaterThan => self.push(Box::new(Boolean {
                value: left_value > right_value,
            })),
            _ => Err(VMError::UnknownOperator),
        }
    }

    fn execute_comparison(&mut self, op: Opcode) -> Result<(), VMError> {
        let right = self.pop();
        let left = self.pop();

        if left.kind() == ObjectType::Integer && right.kind() == ObjectType::Integer {
            return self.execute_integer_comparison(op, left, right);
        }

        match op {
            Opcode::OpEqual => self.push(Box::new(Boolean {
                value: right.get_value() == left.get_value(),
            })),
            Opcode::OpNotEqual => self.push(Box::new(Boolean {
                value: right.get_value() != left.get_value(),
            })),
            _ => Err(VMError::UnknownOperator),
        }
    }

    fn execute_bang_op(&mut self) -> Result<(), VMError> {
        let operand = self.pop();

        match operand.get_value() {
            ObjectValue::Bool(b) => self.push(Box::new(Boolean { value: !b })),
            ObjectValue::Null => self.push(Box::new(Boolean { value: true })),
            _ => self.push(Box::new(Boolean { value: false })),
        }
    }

    fn execute_minus_op(&mut self) -> Result<(), VMError> {
        let operand = self.pop();

        if operand.kind() != ObjectType::Integer {
            return Err(VMError::TypeMismatch);
        }

        let ObjectValue::Int(value) = operand.get_value() else {
            unreachable!();
        };

        self.push(Box::new(Integer { value: -value }))
    }

    fn is_truthy(&self, obj: Box<dyn Object>) -> bool {
        match obj.kind() {
            ObjectType::Boolean => {
                let ObjectValue::Bool(b) = obj.get_value() else {
                    unreachable!()
                };

                return b;
            }

            ObjectType::Null => false,

            _ => true,
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

                Opcode::OpAdd | Opcode::OpSub | Opcode::OpMul | Opcode::OpDiv => {
                    self.execute_binop(op)?
                }

                Opcode::OpPop => {
                    self.pop();
                }

                Opcode::OpTrue => {
                    // todo: figure out how to return the same shared reference,
                    // since booleans are such simple objects
                    self.push(Box::new(Boolean { value: true }))?;
                }

                Opcode::OpFalse => {
                    // todo: figure out how to return the same shared reference,
                    // since booleans are such simple objects
                    self.push(Box::new(Boolean { value: false }))?;
                }

                Opcode::OpEqual | Opcode::OpNotEqual | Opcode::OpGreaterThan => {
                    self.execute_comparison(op)?;
                }

                Opcode::OpBang => self.execute_bang_op()?,

                Opcode::OpMinus => self.execute_minus_op()?,

                Opcode::OpJumpNotTruthy => {
                    let pos = BigEndian::read_u16(&self.instructions[ip + 1..]);
                    ip += 2;

                    let condition = self.pop();
                    if !self.is_truthy(condition) {
                        ip = pos as usize - 1;
                    }
                }

                Opcode::OpJump => {
                    let pos = BigEndian::read_u16(&self.instructions[ip + 1..]);
                    ip = pos as usize - 1;
                }

                Opcode::OpNull => {
                    self.push(Box::new(Null))?;
                }

                Opcode::OpNoop => unreachable!(),
            }

            ip += 1;
        }

        Ok(())
    }
}

#[cfg(test)]
mod vm_tests;
