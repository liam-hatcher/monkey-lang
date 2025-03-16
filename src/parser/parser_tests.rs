use crate::{ast::Statement, lexer::Lexer, parser::Parser};

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
