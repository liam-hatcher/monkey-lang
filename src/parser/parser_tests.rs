use crate::{
    ast::{Expression, Statement},
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

    assert_eq!(program.statements.len(), 1);
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

    assert_eq!(program.statements.len(), 1);

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

#[derive(Debug)]
enum PrefixTestValue {
    Int(i64),
    // String(String),
    // Bool(bool),
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

fn test_literal_expression(expression: &Expression, expected: PrefixTestValue) {
    match expected {
        PrefixTestValue::Int(i) => test_integer_literal(expression, i),
        // PrefixTestValue::String(s) => test_identifier(expression, s),
        // PrefixTestValue::Bool(b) => test_boolean_literal(expression, b),
    }
}

#[test]
fn test_prefix_expressions() {
    let tests_cases: [(String, String, PrefixTestValue); 2] = [
        ("!5;".to_string(), "!".to_string(), PrefixTestValue::Int(5)),
        (
            "-15;".to_string(),
            "-".to_string(),
            PrefixTestValue::Int(15),
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

        assert_eq!(program.statements.len(), 1);

        if let Statement::Expression(es) = &program.statements[0] {
            assert_eq!(es.token.literal, operator);
    
            if let Expression::PrefixExpression(pe) = &*es.expression {
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
