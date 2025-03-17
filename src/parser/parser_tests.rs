use crate::{
    ast::{Expression, InfixExpression, Statement},
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

fn test_identifier(expression: &Expression, expected: String) {
    if let Expression::Identifier(id) = expression {
        assert_eq!(id.value, expected);
        assert_eq!(id.token.literal, expected);
    } else {
        panic!("not an Identifier");
    }
}

fn test_boolean_literal(expression: &Expression, expected: bool) {
    if let Expression::Bool(id) = expression {
        assert_eq!(id.value, expected);
        assert_eq!(id.token.literal, expected.to_string());
    } else {
        panic!("not an Identifier");
    }
}

fn test_literal_expression(exp: &Expression, expected: TestValue) {
    match expected {
        TestValue::String(s) => test_identifier(exp, s),
        TestValue::Bool(b) => test_boolean_literal(exp, b),
        TestValue::Int(i) => test_integer_literal(exp, i.into()),
        // _ => panic!("type of exp not handled. got: {:?} expected", exp),
    }
}

enum TestValue {
    String(String),
    Bool(bool),
    Int(i32),
}

#[test]
fn test_prefix_expressions() {
    let test_cases: [(String, String, TestValue); 6] = [
        ("!5;".to_string(), "!".to_string(), TestValue::Int(5)),
        ("-15;".to_string(), "-".to_string(), TestValue::Int(15)),
        (
            "!foobar;".to_string(),
            "!".to_string(),
            TestValue::String("foobar".to_string()),
        ),
        (
            "-foobar;".to_string(),
            "-".to_string(),
            TestValue::String("foobar".to_string()),
        ),
        ("!true;".to_string(), "!".to_string(), TestValue::Bool(true)),
        (
            "!false;".to_string(),
            "!".to_string(),
            TestValue::Bool(false),
        ),
    ];

    for (input, operator, value) in test_cases {
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

fn test_infix_expression(
    expression: &InfixExpression,
    left: TestValue,
    operator: &String,
    right: TestValue,
) {
    test_literal_expression(&expression.left, left);
    assert_eq!(*operator, expression.operator);
    test_literal_expression(&expression.right, right);
}

#[test]
fn test_infix_expresions() {
    let infix_tests: [(&str, TestValue, &str, TestValue); 19] = [
        ("5 + 5;", TestValue::Int(5), "+", TestValue::Int(5)),
        ("5 - 5;", TestValue::Int(5), "-", TestValue::Int(5)),
        ("5 * 5;", TestValue::Int(5), "*", TestValue::Int(5)),
        ("5 / 5;", TestValue::Int(5), "/", TestValue::Int(5)),
        ("5 > 5;", TestValue::Int(5), ">", TestValue::Int(5)),
        ("5 < 5;", TestValue::Int(5), "<", TestValue::Int(5)),
        ("5 == 5;", TestValue::Int(5), "==", TestValue::Int(5)),
        ("5 != 5;", TestValue::Int(5), "!=", TestValue::Int(5)),
        (
            "foobar + barfoo;",
            TestValue::String("foobar".to_string()),
            "+",
            TestValue::String("barfoo".to_string()),
        ),
        (
            "foobar - barfoo;",
            TestValue::String("foobar".to_string()),
            "-",
            TestValue::String("barfoo".to_string()),
        ),
        (
            "foobar * barfoo;",
            TestValue::String("foobar".to_string()),
            "*",
            TestValue::String("barfoo".to_string()),
        ),
        (
            "foobar / barfoo;",
            TestValue::String("foobar".to_string()),
            "/",
            TestValue::String("barfoo".to_string()),
        ),
        (
            "foobar > barfoo;",
            TestValue::String("foobar".to_string()),
            ">",
            TestValue::String("barfoo".to_string()),
        ),
        (
            "foobar < barfoo;",
            TestValue::String("foobar".to_string()),
            "<",
            TestValue::String("barfoo".to_string()),
        ),
        (
            "foobar == barfoo;",
            TestValue::String("foobar".to_string()),
            "==",
            TestValue::String("barfoo".to_string()),
        ),
        (
            "foobar != barfoo;",
            TestValue::String("foobar".to_string()),
            "!=",
            TestValue::String("barfoo".to_string()),
        ),
        (
            "true == true",
            TestValue::Bool(true),
            "==",
            TestValue::Bool(true),
        ),
        (
            "true != false",
            TestValue::Bool(true),
            "!=",
            TestValue::Bool(false),
        ),
        (
            "false == false",
            TestValue::Bool(false),
            "==",
            TestValue::Bool(false),
        ),
    ];

    for (input, left, operator, right) in infix_tests {
        let mut lexer = Lexer::new(input.into());
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 1, "program length should be 1");

        if let Statement::Expression(es) = &program.statements[0] {
            if let Expression::Infix(ie) = &*es.expression {
                // assert_eq!(ie.operator, operator);
                test_infix_expression(ie, left, &operator.to_string(), right);
            }
        } else {
            panic!("Invalid ExpressionStatement");
        }
    }
}

#[test]
fn test_operator_precedence() {
    let test_cases = [
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        ("true", "true"),
        ("false", "false"),
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 == true", "((3 < 5) == true)"),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("!(true == true)", "(!(true == true))"),
        // ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
        // (
        //     "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
        //     "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        // ),
        // (
        //     "add(a + b + c * d / f + g)",
        //     "add((((a + b) + ((c * d) / f)) + g))",
        // ),
    ];

    for (input, output) in test_cases {
        let mut lexer = Lexer::new(input.into());
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(program.to_string(), output, "Program matches output");
    }
}

#[test]
fn test_if_expression() {
    let input = "if (x < y) { x }";

    let mut lexer = Lexer::new(input.into());
    let mut parser = Parser::new(&mut lexer);
    let program = parser.parse_program();

    assert_eq!(program.statements.len(), 1, "program length should be 1");

    if let Statement::Expression(es) = &program.statements[0] {
        if let Expression::If(ie) = &*es.expression {
            if let Expression::Infix(infix) = &*ie.condition {
                test_infix_expression(
                    infix,
                    TestValue::String("x".into()),
                    &String::from("<"),
                    TestValue::String("y".into()),
                );
            }

            assert_eq!(
                ie.consequence.as_ref().unwrap().statements.len(),
                1,
                "Expected 1 statement"
            );

            if let Statement::Expression(exp) = &ie.consequence.as_ref().unwrap().statements[0] {
                test_identifier(&*exp.expression, String::from("x"));
            }

            assert!(ie.alternative.is_none(), "unexpected alternative");
        } else {
            panic!("Invalid IfExpression");
        }
    } else {
        panic!("Invalid ExpressionStatement");
    }
}
