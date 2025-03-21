use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::ASTNode,
    lexer::Lexer,
    object::{Object, ObjectType, ObjectValue, environment::Environment},
    parser::Parser,
};

use super::eval;

fn test_eval(input: &str) -> Box<dyn Object> {
    let mut lexer = Lexer::new(input.into());
    let mut parser = Parser::new(&mut lexer);
    let program = parser.parse_program();
    let env = Environment::new();

    eval(program, env)
}

fn test_integer_object(obj: &Box<dyn Object>, value: ObjectValue, expected: i64) {
    match obj.kind() {
        ObjectType::Integer => match value {
            ObjectValue::Int(i) => assert_eq!(i, expected, "object value matches expected value"),
            _ => panic!("ObjectValue not an integer"),
        },
        _ => {
            panic!("ObjectType is not an integer, {:?}", obj.kind())
        }
    };
}

#[test]
fn test_eval_integer_expression() {
    let tests = [
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_integer_object(&evaluated, evaluated.get_value(), expected);
    }
}

fn test_boolean_object(obj: &Box<dyn Object>, value: ObjectValue, expected: bool) {
    match obj.kind() {
        ObjectType::Boolean => {
            let ObjectValue::Bool(b) = value else {
                unreachable!("Expected ObjectValue::Bool")
            };
            assert_eq!(b, expected, "Boolean values do not match");
        }
        _ => panic!("ObjectType is not Boolean {:?}", obj.kind()),
    }
}

#[test]
fn test_eval_boolean_expression() {
    let tests = [
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 < 1", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_boolean_object(&evaluated, evaluated.get_value(), expected);
    }
}

#[test]
fn test_bang_operator() {
    let tests = [
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);

        test_boolean_object(&evaluated, evaluated.get_value(), expected);
    }
}

fn is_null_object(obj: &Box<dyn Object>) -> bool {
    match obj.kind() {
        ObjectType::Null => true,
        _ => false,
    }
}

#[test]
fn if_else_expressions() {
    struct TestCase<'a> {
        input: &'a str,
        expected: Option<i64>,
    }

    let tests: Vec<TestCase> = vec![
        TestCase {
            input: "if (true) { 10 }",
            expected: Some(10),
        },
        TestCase {
            input: "if (false) { 10 }",
            expected: None,
        },
        TestCase {
            input: "if (1) { 10 }",
            expected: Some(10),
        },
        TestCase {
            input: "if (1 < 2) { 10 }",
            expected: Some(10),
        },
        TestCase {
            input: "if (1 > 2) { 10 }",
            expected: None,
        },
        TestCase {
            input: "if (1 > 2) { 10 } else { 20 }",
            expected: Some(20),
        },
        TestCase {
            input: "if (1 < 2) { 10 } else { 20 }",
            expected: Some(10),
        },
    ];

    for test in tests {
        let evaluated = test_eval(test.input);
        if let Some(expected) = test.expected {
            test_integer_object(&evaluated, evaluated.get_value(), expected);
        } else {
            assert!(is_null_object(&evaluated));
        }
    }
}

#[test]
fn test_return_statements() {
    let tests = [
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2 * 5; 9;", 10),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_integer_object(&evaluated, evaluated.get_value(), expected);
    }
}

#[test]
fn test_error_handling() {
    let tests = [
        ("5 + true;", "type mismatch: Integer + Boolean"),
        ("5 + true; 5;", "type mismatch: Integer + Boolean"),
        ("-true", "unknown operator: -Boolean"),
        ("true + false;", "unknown operator: Boolean + Boolean"),
        (
            "true + false + true + false;",
            "unknown operator: Boolean + Boolean",
        ),
        ("5; true + false; 5", "unknown operator: Boolean + Boolean"),
        (
            "if (10 > 1) { true + false; }",
            "unknown operator: Boolean + Boolean",
        ),
        (
            "
if (10 > 1) {
  if (10 > 1) {
    return true + false;
  }

  return 1;
}
",
            "unknown operator: Boolean + Boolean",
        ),
        ("foobar", "identifier not found: foobar"),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);

        if evaluated.kind() != ObjectType::Error {
            panic!("Expected Error type, but got {:?}", evaluated.kind());
        }

        assert_eq!(
            evaluated.get_value(),
            ObjectValue::Error(expected.into()),
            "Errors do not match"
        );
    }
}

#[test]
fn test_let_statements() {
    let tests = [
        ("let a = 5; a;", 5),
        ("let a = 5 * 5; a;", 25),
        ("let a = 5; let b = a; b;", 5),
        ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);

        test_integer_object(&evaluated, evaluated.get_value(), expected);
    }
}

#[test]
fn test_function_object() {
    let input = "fn(x) { x + 2; };";

    let evaluated = test_eval(input);

    assert_eq!(
        evaluated.kind(),
        ObjectType::Function,
        "expected a function object"
    );

    let function = evaluated.get_fn_object().unwrap();

    assert!(function.parameters.len() == 1, "one parameter found");

    let param = function.parameters[0].to_string();

    assert_eq!(param, "x", "parameter is not 'x', got {}", param);

    let expected_body = "(x + 2)";

    assert_eq!(
        function.body.to_string(),
        expected_body,
        "function body does not match"
    );
}

#[test]
fn test_function_application() {
    let tests = [
        ("let identity = fn(x) { x; }; identity(5);", 5),
        ("let identity = fn(x) { return x; }; identity(5);", 5),
        ("let double = fn(x) { x * 2; }; double(5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
        ("fn(x) { x; }(5)", 5),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_integer_object(&evaluated, evaluated.get_value(), expected);
    }
}
