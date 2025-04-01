use std::fmt::Write;

use byteorder::{BigEndian, ByteOrder, WriteBytesExt};

pub type Instructions = Vec<u8>;

pub trait InstructionsExt {
    fn string(&self) -> String;
    fn fmt_instruction(&self, def: &Definition, operands: Vec<i32>) -> String;
}

impl InstructionsExt for Instructions {
    fn string(&self) -> String {
        let mut output = String::new();

        let mut i = 0;
        while i < self.len() {
            let inst = self[i];
            let def = Opcode::from_u8(inst).map(lookup_opcode_definition);

            if def.is_none() {
                output += "ERROR\n";
                continue;
            }

            let def = def.unwrap();

            let (operands, read) = read_operands(&def, &self[i + 1..].to_vec());

            writeln!(output, "{:04} {}", i, self.fmt_instruction(&def, operands)).unwrap();

            i += 1 + read as usize;
        }

        output
    }

    fn fmt_instruction(&self, def: &Definition, operands: Vec<i32>) -> String {
        let operand_count = def.operand_widths.len();

        if operand_count != operands.len() {
            return format!(
                "ERROR: operand len {} does not match defined {}\n",
                operands.len(),
                operand_count
            );
        }

        match operand_count {
            0 => def.name.into(),
            1 => format!("{} {}", def.name, operands[0]),
            _ => format!("ERROR: unhandled operand count for {}", def.name),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Opcode {
    OpNoop = 0,
    OpConstant = 1,
    OpAdd = 2,
    OpPop = 3,
    OpSub = 4,
    OpMul = 5,
    OpDiv = 6,
    OpTrue = 7,
    OpFalse = 8,
    OpEqual = 9,
    OpNotEqual = 10,
    OpGreaterThan = 11,
    OpMinus = 12,
    OpBang = 13,
    OpJumpNotTruthy = 14,
    OpJump = 15,
    OpNull = 16,
}

impl Opcode {
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0 => Some(Opcode::OpNoop),
            1 => Some(Opcode::OpConstant),
            2 => Some(Opcode::OpAdd),
            3 => Some(Opcode::OpPop),
            4 => Some(Opcode::OpSub),
            5 => Some(Opcode::OpMul),
            6 => Some(Opcode::OpDiv),
            7 => Some(Opcode::OpTrue),
            8 => Some(Opcode::OpFalse),
            9 => Some(Opcode::OpEqual),
            10 => Some(Opcode::OpNotEqual),
            11 => Some(Opcode::OpGreaterThan),
            12 => Some(Opcode::OpMinus),
            13 => Some(Opcode::OpBang),
            14 => Some(Opcode::OpJumpNotTruthy),
            15 => Some(Opcode::OpJump),
            16 => Some(Opcode::OpNull),
            _ => None,
        }
    }
}

pub struct Definition {
    name: &'static str,
    operand_widths: Vec<i32>,
}

pub fn lookup_opcode_definition(op: Opcode) -> Definition {
    match op {
        Opcode::OpNoop => Definition {
            name: "OpNoop",
            operand_widths: vec![],
        },
        Opcode::OpConstant => Definition {
            name: "OpConstant",
            operand_widths: vec![2],
        },
        Opcode::OpAdd => Definition {
            name: "OpAdd",
            operand_widths: vec![],
        },
        Opcode::OpPop => Definition {
            name: "OpPop",
            operand_widths: vec![],
        },
        Opcode::OpSub => Definition {
            name: "OpSub",
            operand_widths: vec![],
        },
        Opcode::OpMul => Definition {
            name: "OpMul",
            operand_widths: vec![],
        },
        Opcode::OpDiv => Definition {
            name: "OpDiv",
            operand_widths: vec![],
        },
        Opcode::OpTrue => Definition {
            name: "OpTrue",
            operand_widths: vec![],
        },
        Opcode::OpFalse => Definition {
            name: "OpFalse",
            operand_widths: vec![],
        },
        Opcode::OpEqual => Definition {
            name: "OpEqual",
            operand_widths: vec![],
        },
        Opcode::OpNotEqual => Definition {
            name: "OpNotEqual",
            operand_widths: vec![],
        },
        Opcode::OpGreaterThan => Definition {
            name: "OpGreaterThan",
            operand_widths: vec![],
        },
        Opcode::OpMinus => Definition {
            name: "OpMinus",
            operand_widths: vec![],
        },
        Opcode::OpBang => Definition {
            name: "OpBang",
            operand_widths: vec![],
        },
        Opcode::OpJumpNotTruthy => Definition {
            name: "OpJumpNotTruthy",
            operand_widths: vec![2],
        },
        Opcode::OpJump => Definition {
            name: "OpJump",
            operand_widths: vec![2],
        },
        Opcode::OpNull => Definition { name: "OpNull", operand_widths: vec![] }
        // _ => panic!("op code undefined!"), // TODO: should this actually panic?
    }
}

pub fn make(op: Opcode, operands: &[i32]) -> Vec<u8> {
    let definition = lookup_opcode_definition(op);

    let mut instruction_length = 1;
    for w in definition.operand_widths.iter() {
        instruction_length += w;
    }

    let mut instruction = Vec::with_capacity(instruction_length as usize);
    instruction.push(op as u8);

    for (i, o) in operands.iter().enumerate() {
        let width = definition.operand_widths.get(i).unwrap();

        match width {
            2 => {
                instruction.write_u16::<BigEndian>(*o as u16).unwrap();
            }
            _ => panic!("ERROR: unexpected operand width"),
        }
    }

    instruction
}

pub fn read_u16(ins: &[u8]) -> u16 {
    BigEndian::read_u16(&ins)
}

pub fn read_operands(def: &Definition, ins: &Instructions) -> (Vec<i32>, i32) {
    let mut operands = vec![0; def.operand_widths.len()];
    let mut offset = 0;

    for (i, &width) in def.operand_widths.iter().enumerate() {
        match width {
            2 => {
                operands[i] = read_u16(&ins[offset..]) as i32;
            }
            _ => panic!("ERROR: unexpected operand width"), // todo: fix this???
        }

        offset += width as usize;
    }

    (operands, offset as i32)
}

#[cfg(test)]
mod code_tests;
