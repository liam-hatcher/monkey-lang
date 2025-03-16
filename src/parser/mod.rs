use core::fmt;
use std::{cell::RefCell, collections::HashMap, mem};

use crate::{
    ast::*,
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
type InfixParseFn = fn(&mut Parser, Box<Expression>) -> Box<Expression>;

pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Token,
    peek_token: Token,
    // errors: Vec<ParserError>
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: RefCell<HashMap<TokenType, InfixParseFn>>,
}

#[derive(Debug)]
enum ParserError {
    UnknownError,
    UnexpectedToken(String),
    ParseExpressionFail,
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserError::UnexpectedToken(token) => {
                write!(f, "Unexpected token encountered: {}", token)
            }
            ParserError::ParseExpressionFail => write!(f, "Failed to parse expression"),
            ParserError::UnknownError => write!(f, "Unknown error occurred"),
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
            infix_parse_fns: RefCell::new(HashMap::new()),
        };

        parser.register_prefix(TokenType::Identifier, |p| p.parse_identifier());
        parser.register_prefix(TokenType::Int, |p| p.parse_integer_literal());
        parser.register_prefix(TokenType::Bang, |p| p.parse_prefix_expression());
        parser.register_prefix(TokenType::Minus, |p| p.parse_prefix_expression());

        parser.register_infix(TokenType::Plus, |p, ex| p.parse_infix_expression(ex));
        parser.register_infix(TokenType::Minus, |p, ex| p.parse_infix_expression(ex));
        parser.register_infix(TokenType::Slash, |p, ex| p.parse_infix_expression(ex));
        parser.register_infix(TokenType::Asterisk, |p, ex| p.parse_infix_expression(ex));
        parser.register_infix(TokenType::Equal, |p, ex| p.parse_infix_expression(ex));
        parser.register_infix(TokenType::NotEqual, |p, ex| p.parse_infix_expression(ex));
        parser.register_infix(TokenType::LT, |p, ex| p.parse_infix_expression(ex));
        parser.register_infix(TokenType::GT, |p, ex| p.parse_infix_expression(ex));

        // eat two tokens so the current_token and peek_token get set correctly
        parser.next_token();
        parser.next_token();

        parser
    }

    fn register_prefix(&mut self, token_type: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }

    fn register_infix(&mut self, token_type: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.borrow_mut().insert(token_type, func);
    }

    fn get_precedence(&self, tt: &TokenType) -> Option<OperatorPrecedence> {
        match tt {
            TokenType::Equal | TokenType::NotEqual => Some(OperatorPrecedence::Equals),
            TokenType::LT | TokenType::GT => Some(OperatorPrecedence::LessGreater),
            TokenType::Plus | TokenType::Minus => Some(OperatorPrecedence::Sum),
            TokenType::Slash | TokenType::Asterisk => Some(OperatorPrecedence::Product),
            _ => None,
        }
    }

    fn peek_precedence(&self) -> OperatorPrecedence {
        let precedence = self.get_precedence(&self.peek_token.kind);

        if precedence.is_some() {
            precedence.unwrap()
        } else {
            OperatorPrecedence::Lowest
        }
    }

    fn current_precedence(&self) -> OperatorPrecedence {
        let precedence = self.get_precedence(&self.current_token.kind);

        if precedence.is_some() {
            precedence.unwrap()
        } else {
            OperatorPrecedence::Lowest
        }
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

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
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
        let prefix = self.prefix_parse_fns.get(&self.current_token.kind);

        if prefix.is_none() {
            return Err(ParserError::ParseExpressionFail); // should be impossible
        }

        let mut left = prefix.unwrap()(self);

        while self.peek_token.kind != TokenType::Semicolon && precedence < self.peek_precedence() {
            let infix = {
                let infix_fns = self.infix_parse_fns.borrow(); // Borrow immutably
                infix_fns.get(&self.peek_token.kind).cloned() // Retrieve the function, cloned to return by value
            };

            if let Some(infix) = infix {
                self.next_token();
                left = infix(self, left);
            } else {
                return Ok(*left);
            }
        }

        return Ok(*left);
    }

    fn parse_prefix_expression(&mut self) -> Box<Expression> {
        let token = self.current_token.clone();
        let operator = self.current_token.literal.clone();

        self.next_token();

        let right = self.parse_expression(OperatorPrecedence::Prefix).unwrap();

        let expression = PrefixExpression {
            token,
            operator,
            right: Box::new(right),
        };

        Box::new(Expression::Prefix(expression))
    }

    fn parse_infix_expression(&mut self, left: Box<Expression>) -> Box<Expression> {
        let token = self.current_token.clone();
        let operator = self.current_token.literal.clone();
        
        let precedence = self.current_precedence();

        self.next_token();

        let right = self.parse_expression(precedence).unwrap();

        let expression = InfixExpression {
            token,
            operator,
            left,
            right: Box::new(right)
        };

        Box::new(Expression::Infix(expression))
    }

    fn parse_identifier(&mut self) -> Box<Expression> {
        let id = Expression::Identifier(Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        });

        Box::new(id)
    }

    fn parse_integer_literal(&mut self) -> Box<Expression> {
        let value = self.current_token.literal.clone().parse::<i64>();

        if let Ok(int_literal) = value {
            Box::new(Expression::Integer(IntegerLiteral {
                token: self.current_token.clone(),
                value: int_literal,
            }))
        } else {
            panic!("Integer parse failed!");
        }
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

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
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
