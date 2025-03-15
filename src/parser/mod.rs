use crate::{ast::Program, lexer::*, token::Token};

pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Option<Token>, // option type might get annoying here
    peek_token: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        let mut parser = Self {
            lexer,
            current_token: None,
            peek_token: None,
        };

        // Read two tokens, so current_token and peek_token are both set
        parser.next_token();
        parser.next_token();

        return parser;
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token().into();
    }

    pub fn parse_program(&mut self) -> Program {
        todo!()
    }
}

#[cfg(test)]
mod parser_tests;