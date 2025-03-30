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
        "object has wrong value"
    );
}

fn test_expected_object(expected: &Box<dyn Any>, actual: Box<dyn Object>) {
    if let Some(value) = expected.downcast_ref::<i32>() {
        test_integer_object(*value, &actual);
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

        let stack_elem = vm.stack_top().unwrap();
        println!("POOP: {:?}", stack_elem);

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
    ];

    run_vm_tests(&tests);
}
