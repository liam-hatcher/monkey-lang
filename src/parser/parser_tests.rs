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
    let tests = [
        ("let x = 5;", "x", TestValue::Int(5)),
        ("let y = true;", "y", TestValue::Bool(true)),
        ("let foobar = y;", "foobar", TestValue::String("y".to_string())),
    ];
    
    for (input, expected_identifier, expected_value) in tests {
        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 1);

        test_let_statement(&program.statements[0], expected_identifier);
        if let Statement::Let(l) = &program.statements[0] {
            match expected_value {
                // this seems really stupid.
                TestValue::String(s) => {
                    test_literal_expression(&*l.value, TestValue::String(s));
                },
                TestValue::Bool(b) => {
                    test_boolean_literal(&*l.value, b);
                },
                TestValue::Int(i) => {
                    test_literal_expression(&*l.value, TestValue::Int(i));
                },
            };
        }
    }
}

#[test]
fn test_return_statements() {
    let tests = [
        ("return 5;", TestValue::Int(5)),
		("return true;", TestValue::Bool(true)),
		("return foobar;", TestValue::String("foobar".to_string())),
    ];

    for (input, expected) in tests {

        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
    
        assert_eq!(program.statements.len(), 1);

        if let Statement::Return(r) = &program.statements[0] {
            assert!(r.token.literal == "return");
            if r.value.is_some() {
                let expression = *r.value.clone().unwrap();
                let value = match expression {
                    Expression::Integer(i) => TestValue::Int(i.value as i32),
                    Expression::Bool(b) => TestValue::Bool(b.value),
                    Expression::Identifier(i) => TestValue::String(i.value),
                    _ => panic!("Not in test cases")
                };
                assert_eq!(value, expected, "return value is correct");
            }
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

#[derive(Debug, PartialEq)]
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
    assert_eq!(*operator, expression.operator, "infix expression");
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
        ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
        (
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        ),
        (
            "add(a + b + c * d / f + g)",
            "add((((a + b) + ((c * d) / f)) + g))",
        ),
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

#[test]
fn test_function_literal() {
    let input = "fn(x, y) { x + y; }";

    let mut lexer = Lexer::new(input.into());
    let mut parser = Parser::new(&mut lexer);
    let program = parser.parse_program();

    assert_eq!(program.statements.len(), 1, "program length should be 1");

    if let Statement::Expression(es) = &program.statements[0] {
        if let Expression::Function(f) = &*es.expression {
            assert_eq!(f.parameters.len(), 2);
            assert_eq!(f.parameters[0].value, "x", "first parameter matches");
            assert_eq!(f.parameters[1].value, "y", "first parameter matches");

            if let Statement::Expression(es) = &f.body.statements[0] {
                let exp = &*es.expression;
                if let Expression::Infix(ie) = exp {
                    test_infix_expression(
                        ie,
                        TestValue::String("x".into()),
                        &"+".to_string(),
                        TestValue::String("y".into()),
                    );
                } else {
                    panic!("function body not infix expression");
                }
            } else {
                panic!("Invalid function body statement");
            }
        } else {
            panic!("Invalid FunctionLiteral");
        }
    } else {
        panic!("Invalid ExpressionStatement");
    }
}

struct FnLiteralTest {
    input: String,
    expected_params: Vec<String>,
}

#[test]
fn test_parse_function_parameters() {
    let tests = vec![
        FnLiteralTest {
            input: "fn() {};".to_string(),
            expected_params: vec![],
        },
        FnLiteralTest {
            input: "fn(x) {};".to_string(),
            expected_params: vec!["x".to_string()],
        },
        FnLiteralTest {
            input: "fn(x, y, z) {};".to_string(),
            expected_params: vec!["x".to_string(), "y".to_string(), "z".to_string()],
        },
    ];

    for (test) in tests {
        let mut lexer = Lexer::new(test.input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        if let Statement::Expression(es) = &program.statements[0] {
            if let Expression::Function(f) = &*es.expression {
                assert_eq!(
                    f.parameters.len(),
                    test.expected_params.len(),
                    "parameter list lengths match"
                );

                test.expected_params
                    .iter()
                    .enumerate()
                    .for_each(|(i, param)| {
                        assert_eq!(f.parameters[i].value, *param, "function param matches")
                    });
            } else {
                panic!("Invalid FunctionLiteral");
            }
        } else {
            panic!("Invalid ExpressionStatement");
        }
    }
}

#[test]
fn test_call_espression_parsing() {
    let input = "add(1, 2 * 3, 4 + 5);";

    let mut lexer = Lexer::new(input.into());
    let mut parser = Parser::new(&mut lexer);
    let program = parser.parse_program();

    assert_eq!(program.statements.len(), 1, "program length should be 1");

    if let Statement::Expression(es) = &program.statements[0] {
        if let Expression::Call(c) = &*es.expression {
            if let Expression::Identifier(fi) = &*c.function {
                assert_eq!(fi.value, "add", "function identifier should be 'add'");
            } else {
                panic!("expected function identifier")
            }
            assert_eq!(c.arguments.len(), 3, "should have 3 arguments");
            test_literal_expression(&c.arguments[0], TestValue::Int(1));

            if let Expression::Infix(ie) = &*c.arguments[1] {
                test_infix_expression(&ie, TestValue::Int(2), &"*".to_string(), TestValue::Int(3));
            }

            if let Expression::Infix(ie) = &*c.arguments[2] {
                test_infix_expression(&ie, TestValue::Int(4), &"+".to_string(), TestValue::Int(5));
            }
        } else {
            panic!("Invalid CallExpression")
        }
    } else {
        panic!("Invalid ExpressionStatement")
    }
}
