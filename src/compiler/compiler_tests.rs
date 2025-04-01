use std::any::Any;

use crate::{
    ast::Node,
    code::{Instructions, InstructionsExt, Opcode, lookup_opcode_definition, make, read_operands},
    lexer::Lexer,
    object::{Object, ObjectType, ObjectValue},
    parser::Parser,
};

use super::Compiler;

fn parse(input: &str) -> Node {
    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    p.parse_program()
}

fn test_integer_object(expected: i64, actual: &Box<dyn Object>) {
    assert_eq!(actual.kind(), ObjectType::Integer, "object is not integer");

    let value = actual.get_value();
    assert_eq!(value, ObjectValue::Int(expected), "object has wrong value");
}

fn concat_instructions(instructions: Vec<Instructions>) -> Instructions {
    instructions.into_iter().flatten().collect()
}

fn test_instructions(expected: &Vec<Instructions>, actual: Instructions) {
    let concatted = concat_instructions(expected.to_vec());

    assert_eq!(
        actual.len(),
        concatted.len(),
        "wrong instructions length,\n \t expected: {:?}\n \t got:      {:?}",
        concatted.string(),
        actual.string()
    );

    for (i, instr) in concatted.iter().enumerate() {
        assert_eq!(actual.get(i).unwrap(), instr, "instructions do not match");
    }
}

fn test_constants(expected: &Vec<Box<dyn Any>>, actual: Vec<Box<dyn Object>>) {
    assert_eq!(expected.len(), actual.len(), "wrong number of constants");

    for (i, constant) in expected.iter().enumerate() {
        if let Some(value) = constant.downcast_ref::<i64>() {
            test_integer_object(*value, &actual[i]);
        }
    }
}

struct CompilerTestCase {
    input: String,
    expected_constants: Vec<Box<dyn Any>>,
    expected_instructions: Vec<Instructions>,
}

fn run_compiler_tests(tests: &[CompilerTestCase]) {
    for test in tests {
        let program = parse(&test.input);
        let mut compiler = Compiler::new();

        let result = compiler.compile(program);
        if let Err(e) = result {
            panic!("compiler error: {:?}", e);
        };

        let bytecode = compiler.bytecode();
        test_instructions(&test.expected_instructions, bytecode.instructions);
        test_constants(&test.expected_constants, bytecode.constants);
    }
}

#[test]
fn test_integer_arithmetic() {
    let tests = [
        CompilerTestCase {
            input: "1 + 2".into(),
            expected_constants: vec![Box::new(1), Box::new(2)],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpAdd, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTestCase {
            input: "1; 2".into(),
            expected_constants: vec![Box::new(1), Box::new(2)],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpPop, &[]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTestCase {
            input: "1 - 2".into(),
            expected_constants: vec![Box::new(1), Box::new(2)],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpSub, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTestCase {
            input: "1 * 2".into(),
            expected_constants: vec![Box::new(1), Box::new(2)],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpMul, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTestCase {
            input: "2 / 1".into(),
            expected_constants: vec![Box::new(2), Box::new(1)],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpDiv, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTestCase {
            input: "-1".into(),
            expected_constants: vec![Box::new(1)],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpMinus, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
    ];

    run_compiler_tests(&tests);
}

#[test]
fn test_read_operands() {
    struct TestCase<'a> {
        op: Opcode,
        operands: &'a [i32],
        bytes_read: i32,
    }

    let tests = [TestCase {
        op: Opcode::OpConstant,
        operands: &[65535],
        bytes_read: 2,
    }];

    for test in tests {
        let instruction = make(test.op, test.operands);

        let def = lookup_opcode_definition(test.op);

        let (operands_read, n) = read_operands(&def, &instruction[1..].to_vec());
        assert_eq!(n, test.bytes_read, "n wrong");

        for (i, expected) in test.operands.iter().enumerate() {
            assert_eq!(operands_read[i], *expected, "operand wrong");
        }
    }
}

#[test]
fn test_boolean_expressions() {
    let tests = [
        CompilerTestCase {
            input: "true".into(),
            expected_constants: vec![],
            expected_instructions: vec![make(Opcode::OpTrue, &[]), make(Opcode::OpPop, &[])],
        },
        CompilerTestCase {
            input: "false".into(),
            expected_constants: vec![],
            expected_instructions: vec![make(Opcode::OpFalse, &[]), make(Opcode::OpPop, &[])],
        },
        CompilerTestCase {
            input: "1 > 2".into(),
            expected_constants: vec![Box::new(1), Box::new(2)],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpGreaterThan, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTestCase {
            input: "1 < 2".into(),
            expected_constants: vec![Box::new(2), Box::new(1)],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpGreaterThan, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTestCase {
            input: "1 == 2".into(),
            expected_constants: vec![Box::new(1), Box::new(2)],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpEqual, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTestCase {
            input: "1 != 2".into(),
            expected_constants: vec![Box::new(1), Box::new(2)],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpNotEqual, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTestCase {
            input: "true == false".into(),
            expected_constants: vec![],
            expected_instructions: vec![
                make(Opcode::OpTrue, &[]),
                make(Opcode::OpFalse, &[]),
                make(Opcode::OpEqual, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTestCase {
            input: "true != false".into(),
            expected_constants: vec![],
            expected_instructions: vec![
                make(Opcode::OpTrue, &[]),
                make(Opcode::OpFalse, &[]),
                make(Opcode::OpNotEqual, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTestCase {
            input: "!true".into(),
            expected_constants: vec![],
            expected_instructions: vec![
                make(Opcode::OpTrue, &[]),
                make(Opcode::OpBang, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
    ];

    run_compiler_tests(&tests);
}

#[test]
fn test_conditionals() {
    let tests = [
        CompilerTestCase {
            input: "if (true) { 10 }; 3333;".into(),
            expected_constants: vec![Box::new(10), Box::new(3333)],
            expected_instructions: vec![
                // 0000
                make(Opcode::OpTrue, &[]),
                // 0001
                make(Opcode::OpJumpNotTruthy, &[10]),
                // 0004
                make(Opcode::OpConstant, &[0]),
                // 0007
                make(Opcode::OpJump, &[11]),
                // 0010
                make(Opcode::OpNull, &[]),
                // 0011
                make(Opcode::OpPop, &[]),
                // 0012
                make(Opcode::OpConstant, &[1]),
                // 0015
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTestCase {
            input: "if (true) { 10 } else { 20 }; 3333;".into(),
            expected_constants: vec![Box::new(10), Box::new(20), Box::new(3333)],
            expected_instructions: vec![
                // 0000
                make(Opcode::OpTrue, &[]),
                // 0001
                make(Opcode::OpJumpNotTruthy, &[10]),
                // 0004
                make(Opcode::OpConstant, &[0]),
                // 0007
                make(Opcode::OpJump, &[13]),
                // 0010
                make(Opcode::OpConstant, &[1]),
                // 0013
                make(Opcode::OpPop, &[]),
                // 0014
                make(Opcode::OpConstant, &[2]),
                // 0017
                make(Opcode::OpPop, &[]),
            ],
        },
    ];

    run_compiler_tests(&tests);
}
