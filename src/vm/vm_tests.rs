use std::any::Any;

use crate::{
    ast::Node,
    compiler::Compiler,
    lexer::Lexer,
    object::{Object, ObjectType, ObjectValue},
    parser::Parser,
    vm::MonkeyVM,
};

fn parse(input: &str) -> Node {
    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    p.parse_program()
}

fn test_integer_object(expected: i32, actual: &Box<dyn Object>) {
    assert_eq!(actual.kind(), ObjectType::Integer, "object is not integer");

    let value = actual.get_value();
    assert_eq!(
        value,
        ObjectValue::Int(expected as i64),
        "integer object has wrong value"
    );
}

fn test_boolean_object(expected: bool, actual: &Box<dyn Object>) {
    assert_eq!(actual.kind(), ObjectType::Boolean, "object is not boolean");

    let value = actual.get_value();
    assert_eq!(
        value,
        ObjectValue::Bool(expected),
        "boolean object has wrong value"
    );
}

fn test_expected_object(expected: &Box<dyn Any>, actual: Box<dyn Object>) {
    if let Some(value) = expected.downcast_ref::<i32>() {
        test_integer_object(*value, &actual);
    } else if let Some(value) = expected.downcast_ref::<bool>() {
        test_boolean_object(*value, &actual);
    } else {
        unreachable!();
    }
}

struct VMTestCase {
    input: String,
    expected: Box<dyn Any>,
}

fn run_vm_tests(tests: &[VMTestCase]) {
    for test in tests {
        let program = parse(&test.input);

        let mut compiler = Compiler::new();
        let result = compiler.compile(program);
        if let Err(e) = result {
            panic!("compiler error: {:?}", e);
        };

        let mut vm = MonkeyVM::new(compiler.bytecode());

        let result = vm.run();

        assert!(result.is_ok(), "vm error: {}", result.err().unwrap());

        let stack_elem = vm.last_popped_stack_elem();

        test_expected_object(&test.expected, stack_elem.clone());
    }
}

#[test]
fn test_integer_arithmetic() {
    let tests = [
        VMTestCase {
            input: "1".into(),
            expected: Box::new(1),
        },
        VMTestCase {
            input: "2".into(),
            expected: Box::new(2),
        },
        VMTestCase {
            input: "1 + 2".into(),
            expected: Box::new(3),
        },
        VMTestCase {
            input: "1 - 2".into(),
            expected: Box::new(-1),
        },
        VMTestCase {
            input: "1 * 2".into(),
            expected: Box::new(2),
        },
        VMTestCase {
            input: "4 / 2".into(),
            expected: Box::new(2),
        },
        VMTestCase {
            input: "50 / 2 * 2 + 10 - 5".into(),
            expected: Box::new(55),
        },
        VMTestCase {
            input: "5 + 5 + 5 + 5 - 10".into(),
            expected: Box::new(10),
        },
        VMTestCase {
            input: "2 * 2 * 2 * 2 * 2".into(),
            expected: Box::new(32),
        },
        VMTestCase {
            input: "5 * 2 + 10".into(),
            expected: Box::new(20),
        },
        VMTestCase {
            input: "5 + 2 * 10".into(),
            expected: Box::new(25),
        },
        VMTestCase {
            input: "5 * (2 + 10)".into(),
            expected: Box::new(60),
        },
        VMTestCase {
            input: "-5".into(),
            expected: Box::new(-5),
        },
        VMTestCase {
            input: "-10".into(),
            expected: Box::new(-10),
        },
        VMTestCase {
            input: "-50 + 100 + -50".into(),
            expected: Box::new(0),
        },
        VMTestCase {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".into(),
            expected: Box::new(50),
        },
    ];

    run_vm_tests(&tests);
}

#[test]
fn test_boolean_expressions() {
    let tests = [
        VMTestCase {
            input: "true".into(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "false".into(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "1 < 2".into(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "1 > 2".into(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "1 < 1".into(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "1 > 1".into(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "1 == 1".into(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "1 != 1".into(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "1 == 2".into(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "1 != 2".into(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "true == true".into(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "false == false".into(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "true == false".into(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "true != false".into(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "false != true".into(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "(1 < 2) == true".into(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "(1 < 2) == false".into(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "(1 > 2) == true".into(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "(1 > 2) == false".into(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "!true".into(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "!false".into(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "!5".into(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "!!true".into(),
            expected: Box::new(true),
        },
        VMTestCase {
            input: "!!false".into(),
            expected: Box::new(false),
        },
        VMTestCase {
            input: "!!5".into(),
            expected: Box::new(true),
        },
    ];

    run_vm_tests(&tests);
}
