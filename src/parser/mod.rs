use core::fmt;
use std::collections::HashMap;

use crate::{
    ast::{Expression, ExpressionStatement, Identifier, Let, Program, Return, Statement},
    lexer::Lexer,
    token::{Token, TokenType},
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
pub enum OperatorPrecedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

type PrefixParseFn = fn(&mut Parser) -> Box<Expression>;

pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Token,
    peek_token: Token,
    // errors: Vec<ParserError>
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
}

#[derive(Debug)]
enum ParserError {
    UnknownError,
    UnexpectedToken(String),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserError::UnknownError => write!(f, "Unknown error occurred"),
            ParserError::UnexpectedToken(token) => {
                write!(f, "Unexpected token encountered: {}", token)
            }
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        let mut parser: Parser<'a> = Self {
            lexer,
            current_token: Token::default(),
            peek_token: Token::default(),
            // errors: Vec::default(),
            prefix_parse_fns: HashMap::new(),
        };

        parser.register_prefix(TokenType::Identifier, |p| p.parse_identifier());

        // eat two tokens so the current_token and peek_token get set correctly
        parser.next_token();
        parser.next_token();

        parser
    }

    fn register_prefix(&mut self, token_type: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token().into();
    }

    /*
      Checks to see if the next token matches the provided 'kind',
      and "eats" the next token if so. Otherwise produces an error.
    */
    fn expect_peek(&mut self, expected_kind: TokenType) -> Result<(), ParserError> {
        if self.peek_token.kind == expected_kind {
            self.next_token();
            return Ok(());
        }

        Err(ParserError::UnexpectedToken(
            self.peek_token.literal.clone(),
        ))
    }

    pub fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        let let_token = self.current_token.clone();

        if let Err(e) = self.expect_peek(TokenType::Identifier) {
            return Err(e);
        }

        let id = Identifier {
            token: self.current_token.clone(),
            value: String::from(self.current_token.clone().literal),
        };

        if let Err(e) = self.expect_peek(TokenType::Assign) {
            return Err(e);
        }

        // TODO: We're skipping the expressions until we
        // encounter a semicolon
        if self.current_token.kind != TokenType::Semicolon {
            self.next_token();
            self.next_token(); // TODO: remove me when we parse expressions.... i think... idk why I have to eat 2 tokens here? bug maybe?
        }

        Ok(Statement::Let(Let {
            token: let_token,
            id,
            // value: TODO once we add expressions
        }))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.current_token.clone();

        self.next_token();

        // TODO: We're skipping the expressions until we
        // encounter a semicolon
        if self.current_token.kind != TokenType::Semicolon {
            self.next_token();
        }

        let statement = Return { token };

        Ok(Statement::Return(statement))
    }

    fn parse_expression(
        &mut self,
        precedence: OperatorPrecedence,
    ) -> Result<Expression, ParserError> {
        if let Some(prefix_parse) = self.prefix_parse_fns.get(&self.current_token.kind) {
            return Ok(*prefix_parse(self));
        }
        Err(ParserError::UnknownError)
    }

    fn parse_identifier(&mut self) -> Box<Expression> {
        let id = Expression::Identifier(Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        });

        Box::new(id)
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.current_token.clone();

        let expression = self.parse_expression(OperatorPrecedence::Lowest).unwrap();

        if self.peek_token.kind == TokenType::Semicolon {
            self.next_token();
        }

        let expr_statement = ExpressionStatement {
            token,
            expression: Box::new(expression),
        };

        Ok(Statement::Expression(expr_statement))
    }

    pub fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.current_token.kind {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Statement> = vec![];

        while self.current_token.clone().kind != TokenType::EOF {
            let statement = self.parse_statement();
            if statement.is_ok() {
                statements.push(statement.unwrap());
            } else if let Err(e) = statement {
                panic!("ERROR!\n {}", e);
            }

            self.next_token();
        }

        Program { statements }
    }
}

#[cfg(test)]
mod parser_tests;
