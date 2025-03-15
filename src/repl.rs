use std::io::{self, Write};

use crate::{lexer::Lexer, token::TokenType};

pub fn start_repl() {
    print!("monkey >>");

    io::stdout().flush().unwrap();

    let mut input = String::new();

    io::stdin().read_line(&mut input).unwrap();

    let mut lexer = Lexer::new(input);

    loop {
        let t = lexer.next_token();
        if t.kind == TokenType::EOF || t.kind == TokenType::Illegal {
            break;
        }
        println!("{:?}", t);
    }
}
