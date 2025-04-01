use std::{error::Error, fmt};

use crate::{
    ast::{Expression, Node, Statement},
    code::{Instructions, Opcode, make},
    object::{Integer, Object},
};

#[derive(Debug)]
pub enum CompilerError {
    UnknownOperator(String),
}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompilerError::UnknownOperator(s) => write!(f, "unknown operator: {}", s),
        }
    }
}

impl std::error::Error for CompilerError {}

#[derive(Debug, Clone)]
pub struct EmittedInstruction {
    pub op_code: Opcode,
    pub position: i32,
}

pub struct ByteCode {
    pub instructions: Instructions,
    pub constants: Vec<Box<dyn Object>>,
}

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Box<dyn Object>>,
    last_instruction: EmittedInstruction,
    previous_instruction: EmittedInstruction,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Instructions::new(),
            constants: vec![],
            last_instruction: EmittedInstruction {
                op_code: Opcode::OpNoop,
                position: -1,
            },
            previous_instruction: EmittedInstruction {
                op_code: Opcode::OpNoop,
                position: -1,
            },
        }
    }

    fn add_constant(&mut self, obj: Box<dyn Object>) -> i32 {
        self.constants.push(obj);

        self.constants.len() as i32 - 1
    }

    fn add_instruction(&mut self, instructions: Instructions) -> i32 {
        let pos_new_instruction = self.instructions.len();
        self.instructions.extend(instructions);

        pos_new_instruction as i32
    }

    fn set_last_instruction(&mut self, op_code: Opcode, position: i32) {
        let previous = self.last_instruction.clone();
        let last = EmittedInstruction { op_code, position };

        self.previous_instruction = previous;
        self.last_instruction = last;
    }

    fn remove_last_pop(&mut self) {
        self.instructions
            .truncate(self.last_instruction.position as usize);

        self.last_instruction = self.previous_instruction.clone();
    }

    fn change_operand(&mut self, op_pos: i32, operand: i32) {
        let op = Opcode::from_u8(self.instructions[op_pos as usize]).unwrap();
        let new_instruction = make(op, &[operand]);

        self.replace_instruction(op_pos, new_instruction);
    }

    fn replace_instruction(&mut self, position: i32, new_instruction: Vec<u8>) {
        for i in 0..new_instruction.len() {
            self.instructions[position as usize + i] = new_instruction[i]
        }
    }

    fn emit(&mut self, op: Opcode, operands: &[i32]) -> i32 {
        let instruction = make(op, operands);

        let position = self.add_instruction(instruction);

        self.set_last_instruction(op, position);

        position
    }

    pub fn compile(&mut self, node: Node) -> Result<(), CompilerError> {
        match node {
            Node::Program(program) => {
                for s in program.statements {
                    self.compile(Node::Statement(s))?;
                }

                Ok(())
            }

            Node::Statement(statement) => match statement {
                Statement::Expression(expression_statement) => {
                    self.compile(Node::Expression(*expression_statement.expression))?;

                    self.emit(Opcode::OpPop, &[]);

                    Ok(())
                }

                Statement::Block(block_statement) => {
                    for s in *block_statement.statements {
                        self.compile(Node::Statement(s))?;
                    }

                    Ok(())
                }

                Statement::Let(let_statement) => todo!(),
                Statement::Return(return_statement) => todo!(),
            },

            Node::Expression(expression) => match expression {
                Expression::Infix(infix_expression) => {
                    if infix_expression.operator == "<" {
                        self.compile(Node::Expression(*infix_expression.right))?;

                        self.compile(Node::Expression(*infix_expression.left))?;

                        self.emit(Opcode::OpGreaterThan, &[]);

                        return Ok(());
                    }

                    self.compile(Node::Expression(*infix_expression.left))?;

                    self.compile(Node::Expression(*infix_expression.right))?;

                    match infix_expression.operator.as_str() {
                        "+" => self.emit(Opcode::OpAdd, &[]),
                        "-" => self.emit(Opcode::OpSub, &[]),
                        "*" => self.emit(Opcode::OpMul, &[]),
                        "/" => self.emit(Opcode::OpDiv, &[]),
                        ">" => self.emit(Opcode::OpGreaterThan, &[]),
                        "==" => self.emit(Opcode::OpEqual, &[]),
                        "!=" => self.emit(Opcode::OpNotEqual, &[]),
                        _ => return Err(CompilerError::UnknownOperator(infix_expression.operator)),
                    };

                    Ok(())
                }

                Expression::Integer(integer_literal) => {
                    let integer = Integer {
                        value: integer_literal.value,
                    };

                    let operands = self.add_constant(Box::new(integer));

                    self.emit(Opcode::OpConstant, &[operands]);

                    Ok(())
                }

                Expression::Bool(boolean_expression) => {
                    if boolean_expression.value {
                        self.emit(Opcode::OpTrue, &[]);
                    } else {
                        self.emit(Opcode::OpFalse, &[]);
                    }

                    Ok(())
                }

                Expression::Prefix(prefix_expression) => {
                    self.compile(Node::Expression(*prefix_expression.right))?;

                    match prefix_expression.operator.as_str() {
                        "!" => self.emit(Opcode::OpBang, &[]),
                        "-" => self.emit(Opcode::OpMinus, &[]),
                        _ => {
                            return Err(CompilerError::UnknownOperator(prefix_expression.operator));
                        }
                    };

                    Ok(())
                }

                Expression::If(if_expression) => {
                    self.compile(Node::Expression(*if_expression.condition))?;

                    // Emit an `OpJumpNotTruthy` with a bogus value
                    let jump_not_truthy_pos = self.emit(Opcode::OpJumpNotTruthy, &[9999]);

                    let consequence = if_expression.consequence;

                    if consequence.is_some() {
                        self.compile(Node::Statement(Statement::Block(*consequence.unwrap())))?;
                    }

                    if self.last_instruction.op_code == Opcode::OpPop {
                        self.remove_last_pop();
                    }

                    // Emit an `OpJump` with a bogus value
                    let jump_pos = self.emit(Opcode::OpJump, &[9999]);

                    let after_consequence_pos = self.instructions.len();
                    self.change_operand(jump_not_truthy_pos, after_consequence_pos as i32);

                    if if_expression.alternative.is_none() {
                        self.emit(Opcode::OpNull, &[]);
                    } else {
                        self.compile(Node::Statement(Statement::Block(
                            *if_expression.alternative.unwrap(),
                        )))?;

                        if self.last_instruction.op_code == Opcode::OpPop {
                            self.remove_last_pop();
                        }
                    }

                    let after_alternative_pos = self.instructions.len();
                    self.change_operand(jump_pos, after_alternative_pos as i32);

                    Ok(())
                }

                Expression::Identifier(identifier_expression) => todo!(),
                Expression::Function(function_literal) => todo!(),
                Expression::Call(call_expression) => todo!(),
                Expression::String(string_literal) => todo!(),
                Expression::Array(array_literal) => todo!(),
                Expression::Index(index_expression) => todo!(),
                Expression::Hash(hash_literal) => todo!(),
            },
        }
    }

    pub fn bytecode(&self) -> ByteCode {
        ByteCode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }
}

#[cfg(test)]
mod compiler_tests;
