use crate::token::TokenType;

use super::Lexer;

#[test]
fn test_next_token() {
    let input = r#"
            let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
            "foobar"
            "foo bar"
            [1,2];
            {"foo": "bar"}
        "#;

    let mut l = Lexer::new(input.into());

    let test_cases = [
        (TokenType::Let, "let"),
        (TokenType::Identifier, "five"),
        (TokenType::Assign, "="),
        (TokenType::Int, "5"),
        (TokenType::Semicolon, ";"),
        (TokenType::Let, "let"),
        (TokenType::Identifier, "ten"),
        (TokenType::Assign, "="),
        (TokenType::Int, "10"),
        (TokenType::Semicolon, ";"),
        (TokenType::Let, "let"),
        (TokenType::Identifier, "add"),
        (TokenType::Assign, "="),
        (TokenType::Function, "fn"),
        (TokenType::LParen, "("),
        (TokenType::Identifier, "x"),
        (TokenType::Comma, ","),
        (TokenType::Identifier, "y"),
        (TokenType::RParen, ")"),
        (TokenType::LBrace, "{"),
        (TokenType::Identifier, "x"),
        (TokenType::Plus, "+"),
        (TokenType::Identifier, "y"),
        (TokenType::Semicolon, ";"),
        (TokenType::RBrace, "}"),
        (TokenType::Semicolon, ";"),
        (TokenType::Let, "let"),
        (TokenType::Identifier, "result"),
        (TokenType::Assign, "="),
        (TokenType::Identifier, "add"),
        (TokenType::LParen, "("),
        (TokenType::Identifier, "five"),
        (TokenType::Comma, ","),
        (TokenType::Identifier, "ten"),
        (TokenType::RParen, ")"),
        (TokenType::Semicolon, ";"),
        (TokenType::Bang, "!"),
        (TokenType::Minus, "-"),
        (TokenType::Slash, "/"),
        (TokenType::Asterisk, "*"),
        (TokenType::Int, "5"),
        (TokenType::Semicolon, ";"),
        (TokenType::Int, "5"),
        (TokenType::LT, "<"),
        (TokenType::Int, "10"),
        (TokenType::GT, ">"),
        (TokenType::Int, "5"),
        (TokenType::Semicolon, ";"),
        (TokenType::If, "if"),
        (TokenType::LParen, "("),
        (TokenType::Int, "5"),
        (TokenType::LT, "<"),
        (TokenType::Int, "10"),
        (TokenType::RParen, ")"),
        (TokenType::LBrace, "{"),
        (TokenType::Return, "return"),
        (TokenType::True, "true"),
        (TokenType::Semicolon, ";"),
        (TokenType::RBrace, "}"),
        (TokenType::Else, "else"),
        (TokenType::LBrace, "{"),
        (TokenType::Return, "return"),
        (TokenType::False, "false"),
        (TokenType::Semicolon, ";"),
        (TokenType::RBrace, "}"),
        (TokenType::Int, "10"),
        (TokenType::Equal, "=="),
        (TokenType::Int, "10"),
        (TokenType::Semicolon, ";"),
        (TokenType::Int, "10"),
        (TokenType::NotEqual, "!="),
        (TokenType::Int, "9"),
        (TokenType::Semicolon, ";"),
        (TokenType::Str, "foobar"),
        (TokenType::Str, "foo bar"),
        (TokenType::LBracket, "["),
        (TokenType::Int, "1"),
        (TokenType::Comma, ","),
        (TokenType::Int, "2"),
        (TokenType::RBracket, "]"),
        (TokenType::Semicolon, ";"),
        (TokenType::LBrace, "{"),
        (TokenType::Str, "foo"),
        (TokenType::Colon, ":"),
        (TokenType::Str, "bar"),
        (TokenType::RBrace, "}"),
        (TokenType::EOF, ""),
    ];

    for (tt, literal) in test_cases {
        let token = l.next_token();

        assert_eq!(tt, token.kind);
        assert_eq!(literal, token.literal);
    }
}
