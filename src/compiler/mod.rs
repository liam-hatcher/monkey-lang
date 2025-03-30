use std::{error::Error, fmt};

use crate::{
    ast::{Expression, Node, Statement},
    code::{Instructions, Opcode, make},
    object::{Integer, Object},
};

#[derive(Debug)]
pub enum CompilerError {
    UnknownOperator,
}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CompilerError::UnknownOperator => write!(f, "unknown operator!"),
        }
    }
}

impl std::error::Error for CompilerError {}

pub struct ByteCode {
    pub instructions: Instructions,
    pub constants: Vec<Box<dyn Object>>,
}

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Box<dyn Object>>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Instructions::new(),
            constants: vec![],
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

    fn emit(&mut self, op: Opcode, operands: &[i32]) -> i32 {
        let instruction = make(op, operands);

        let position = self.add_instruction(instruction);

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

                    Ok(())
                }

                Statement::Let(let_statement) => todo!(),
                Statement::Return(return_statement) => todo!(),
                Statement::Block(block_statement) => todo!(),
            },

            Node::Expression(expression) => match expression {
                Expression::Infix(infix_expression) => {
                    self.compile(Node::Expression(*infix_expression.left))?;

                    self.compile(Node::Expression(*infix_expression.right))?;

                    match infix_expression.operator.as_str() {
                        "+" => {
                            self.emit(Opcode::OpAdd, &[]);

                            Ok(())
                        }
                        _ => Err(CompilerError::UnknownOperator),
                    }
                }

                Expression::Integer(integer_literal) => {
                    let integer = Integer {
                        value: integer_literal.value,
                    };

                    let operands = self.add_constant(Box::new(integer));

                    self.emit(Opcode::OpConstant, &[operands]);

                    Ok(())
                }

                Expression::Identifier(identifier_expression) => todo!(),
                Expression::Prefix(prefix_expression) => todo!(),
                Expression::Bool(boolean_expression) => todo!(),
                Expression::If(if_expression) => todo!(),
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
