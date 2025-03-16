use crate::{ast::{Expression, Statement}, lexer::Lexer, parser::Parser};

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

    let tests = [
        "x",
        "y",
        "foobar"
    ];

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
        }
    } else {
        panic!("Invalid IntegerLiteral");
    }
}

// func TestLetStatements(t *testing.T) {
// 	tests := []struct {
// 		input              string
// 		expectedIdentifier string
// 		expectedValue      interface{}
// 	}{
// 		{"let x = 5;", "x", 5},
// 		{"let y = true;", "y", true},
// 		{"let foobar = y;", "foobar", "y"},
// 	}

// 	for _, tt := range tests {
// 		l := lexer.New(tt.input)
// 		p := New(l)
// 		program := p.ParseProgram()
// 		checkParserErrors(t, p)

// 		if len(program.Statements) != 1 {
// 			t.Fatalf("program.Statements does not contain 1 statements. got=%d",
// 				len(program.Statements))
// 		}

// 		stmt := program.Statements[0]
// 		if !testLetStatement(t, stmt, tt.expectedIdentifier) {
// 			return
// 		}

// 		val := stmt.(*ast.LetStatement).Value
// 		if !testLiteralExpression(t, val, tt.expectedValue) {
// 			return
// 		}
// 	}
// }
