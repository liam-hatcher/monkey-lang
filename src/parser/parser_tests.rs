use std::any::Any;

use crate::{
    ast::{Expression, Statement, InfixExpression},
    lexer::Lexer,
    parser::Parser,
};

fn test_let_statement(statement: &Statement, expected_id: &str) {
    if let Statement::Let(s) = statement {
        assert_eq!(s.token.literal, "let");
        assert_eq!(s.id.value, expected_id);
    } else {
        panic!("invalid let statement: {}", expected_id);
    }
}

#[test]
fn test_let_statements() {
    let input = r#"
        let x = 5;
        let y = 10;
        let foobar = 838383;
    "#;

    let mut lexer = Lexer::new(input.into());
    let mut parser = Parser::new(&mut lexer);

    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 3);

    let tests = ["x", "y", "foobar"];

    for (i, test) in tests.iter().enumerate() {
        test_let_statement(&program.statements[i], test);
    }
}

#[test]
fn test_return_statements() {
    let input = r#"
        return 5;
        return 10;
        return 993322;
    "#;

    let mut lexer = Lexer::new(input.into());
    let mut parser = Parser::new(&mut lexer);
    let program = parser.parse_program();

    assert_eq!(program.statements.len(), 3);

    for s in program.statements {
        if let Statement::Return(r) = s {
            assert!(r.token.literal == "return");
        } else {
            panic!("invalid return statement!");
        }
    }
}

#[test]
fn test_identifier_expression() {
    let input = "foobar;";

    let mut lexer = Lexer::new(input.into());
    let mut parser = Parser::new(&mut lexer);
    let program = parser.parse_program();

    assert_eq!(program.statements.len(), 1, "program length should be 1");
    if let Statement::Expression(es) = &program.statements[0] {
        assert_eq!(es.token.literal, "foobar");

        if let Expression::Identifier(id) = &*es.expression {
            assert_eq!(id.value, "foobar");
        } else {
            panic!("Invalid Identifier")
        }
    } else {
        panic!("Invalid ExpressionStatement");
    }
}

#[test]
fn test_integer_literal_expression() {
    let input = "5;";

    let mut lexer = Lexer::new(input.into());
    let mut parser = Parser::new(&mut lexer);
    let program = parser.parse_program();

    assert_eq!(program.statements.len(), 1, "program length should be 1");

    if let Statement::Expression(es) = &program.statements[0] {
        assert_eq!(es.token.literal, "5");

        if let Expression::Integer(id) = &*es.expression {
            assert_eq!(id.value, 5);
        } else {
            panic!("Invalid IntegerLiteral");
        }
    } else {
        panic!("Invalid ExpressionStatement");
    }
}

fn test_integer_literal(expression: &Expression, expected: i64) {
    if let Expression::Integer(num) = expression {
        assert_eq!(num.value, expected);
        assert_eq!(num.token.literal, expected.to_string());
    } else {
        panic!("not an IntegerLiteral");
    }
}

// fn test_identifier(expression: &Expression, expected: String) {
//     if let Expression::Identifier(id) = expression {
//         assert_eq!(id.value, expected);
//         assert_eq!(id.token.literal, expected);
//     } else {
//         panic!("not an Identifier");
//     }
// }

// fn test_boolean_literal(expression: &Expression, expected: bool) {
//     if let Expression::Boolean(id) = expression {
//         assert_eq!(id.value, expected);
//         assert_eq!(id.token.literal, expected);
//     } else {
//         panic!("not an Identifier");
//     }
// }

fn test_literal_expression(expression: &Expression, expected: &dyn Any) {
    if let Some(&int_value) = expected.downcast_ref::<i64>() {
        if let Expression::Integer(i) = expression {
            assert_eq!(i.value, int_value);
        } else {
            panic!("Expected an Integer expression");
        }
    } else if let Some(&int_value) = expected.downcast_ref::<i32>() {
        if let Expression::Integer(i) = expression {
            assert_eq!(i.value, int_value.into());
        } else {
            panic!("Expected an Integer expression");
        }
    } else if let Some(string_value) = expected.downcast_ref::<String>() {
        if let Expression::Identifier(id) = expression {
            assert_eq!(&id.value, string_value);
        } else {
            panic!("Expected an Identifier expression");
        }
    } else if let Some(&bool_value) = expected.downcast_ref::<bool>() {
        // if let Expression::Boolean(b) = expression {
        //     assert_eq!(b.value, bool_value);
        // } else {
        //     panic!("Expected a Boolean expression");
        // }
        panic!("NOT IMPLEMENTED YET");
    } else {
        panic!("Unexpected type for expected value");
    }
}

#[test]
fn test_prefix_expressions() {
    let tests_cases: [(String, String, &dyn Any); 2] = [
        ("!5;".to_string(), "!".to_string(), &5),
        (
            "-15;".to_string(),
            "-".to_string(),
            &15,
        ),
        // (
        //     "!foobar;".to_string(),
        //     "!".to_string(),
        //     PrefixTestValue::String("foobar".to_string()),
        // ),
        // (
        //     "-foobar;".to_string(),
        //     "-".to_string(),
        //     PrefixTestValue::String("foobar".to_string()),
        // ),
        // (
        //     "!true;".to_string(),
        //     "!".to_string(),
        //     PrefixTestValue::Bool(true),
        // ),
        // (
        //     "!false;".to_string(),
        //     "!".to_string(),
        //     PrefixTestValue::Bool(false),
        // ),
    ];

    for (input, operator, value) in tests_cases {
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 1, "program length should be 1");

        if let Statement::Expression(es) = &program.statements[0] {
            assert_eq!(es.token.literal, operator);

            if let Expression::Prefix(pe) = &*es.expression {
                assert_eq!(pe.operator, operator);
                test_literal_expression(&*pe.right, value);
            } else {
                panic!("Invalid PrefixExpression");
            }
        } else {
            panic!("Invalid ExpressionStatement");
        }
    }
}

fn test_infix_expression(expression: &InfixExpression, left: i64, operator: &String, right: i64) {
    test_literal_expression(&expression.left, &left);
    assert_eq!(*operator, expression.operator);
    test_literal_expression(&expression.right, &right);
}

#[test]
fn test_infix_expresions() {
    let infix_tests: &[(String, i64, String, i64)] = &[
        ("5 + 5;".to_string(), 5, "+".to_string(), 5),
        ("5 - 5;".to_string(), 5, "-".to_string(), 5),
        ("5 * 5;".to_string(), 5, "*".to_string(), 5),
        ("5 / 5;".to_string(), 5, "/".to_string(), 5),
        ("5 > 5;".to_string(), 5, ">".to_string(), 5),
        ("5 < 5;".to_string(), 5, "<".to_string(), 5),
        ("5 == 5;".to_string(), 5, "==".to_string(), 5),
        ("5 != 5;".to_string(), 5, "!=".to_string(), 5),
    ];

    for (input, left, operator, right) in infix_tests {
        let mut lexer = Lexer::new(input.into());
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 1, "program length should be 1");

        if let Statement::Expression(es) = &program.statements[0] {
            if let Expression::Infix(ie) = &*es.expression {
                // assert_eq!(ie.operator, operator);
                test_infix_expression(ie, *left, operator, *right );
            }
        } else {
            panic!("Invalid ExpressionStatement");
        }
    }
}
