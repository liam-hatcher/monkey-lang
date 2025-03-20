use core::fmt;
use std::{cell::RefCell, collections::HashMap};

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
type PrefixOptionalParseFn = fn(&mut Parser) -> Option<Box<Expression>>;

// This feels really clumsy.
enum ParseFn {
    Prefix(PrefixParseFn),
    PrefixOptional(PrefixOptionalParseFn),
}
type InfixParseFn = fn(&mut Parser, Box<Expression>) -> Box<Expression>;

pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Token,
    peek_token: Token,
    prefix_parse_fns: HashMap<TokenType, ParseFn>,
    // This seems like a hack, but i'm a n00b so ¯\_(ツ)_/¯
    infix_parse_fns: RefCell<HashMap<TokenType, InfixParseFn>>,

    pub errors: Vec<String>,
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
        use TokenType::*;

        let mut parser: Parser<'a> = Self {
            lexer,
            current_token: Token::default(),
            peek_token: Token::default(),
            // errors: Vec::default(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: RefCell::new(HashMap::new()),
            errors: vec![],
        };

        parser.register_prefix(Identifier, ParseFn::Prefix(|p| p.parse_identifier()));
        parser.register_prefix(Int, ParseFn::PrefixOptional(|p| p.parse_integer_literal()));
        parser.register_prefix(Bang, ParseFn::Prefix(|p| p.parse_prefix_expression()));
        parser.register_prefix(Minus, ParseFn::Prefix(|p| p.parse_prefix_expression()));
        parser.register_prefix(True, ParseFn::Prefix(|p| p.parse_boolean()));
        parser.register_prefix(False, ParseFn::Prefix(|p| p.parse_boolean()));
        parser.register_prefix(
            LParen,
            ParseFn::PrefixOptional(|p| p.parse_grouped_expression()),
        );
        parser.register_prefix(If, ParseFn::PrefixOptional(|p| p.parse_if_expression()));
        parser.register_prefix(
            Function,
            ParseFn::PrefixOptional(|p| p.parse_function_literal()),
        );

        parser.register_infix(Plus, |p, ex| p.parse_infix_expression(ex));
        parser.register_infix(Minus, |p, ex| p.parse_infix_expression(ex));
        parser.register_infix(Slash, |p, ex| p.parse_infix_expression(ex));
        parser.register_infix(Asterisk, |p, ex| p.parse_infix_expression(ex));
        parser.register_infix(Equal, |p, ex| p.parse_infix_expression(ex));
        parser.register_infix(NotEqual, |p, ex| p.parse_infix_expression(ex));
        parser.register_infix(LT, |p, ex| p.parse_infix_expression(ex));
        parser.register_infix(GT, |p, ex| p.parse_infix_expression(ex));
        parser.register_infix(LParen, |p, ex| p.parse_call_expression(ex));

        // eat two tokens so the current_token and peek_token get set correctly
        parser.next_token();
        parser.next_token();

        parser
    }

    fn register_prefix(&mut self, token_type: TokenType, func: ParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }

    fn register_infix(&mut self, token_type: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.borrow_mut().insert(token_type, func);
    }

    fn get_precedence(&self, tt: &TokenType) -> Option<OperatorPrecedence> {
        use TokenType::*;
        match tt {
            Equal | NotEqual => Some(OperatorPrecedence::Equals),
            LT | GT => Some(OperatorPrecedence::LessGreater),
            Plus | Minus => Some(OperatorPrecedence::Sum),
            Slash | Asterisk => Some(OperatorPrecedence::Product),
            LParen => Some(OperatorPrecedence::Call),
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
    fn expect_peek(&mut self, expected_kind: TokenType) -> bool {
        if self.peek_token.kind == expected_kind {
            self.next_token();
            return true;
        }

        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead",
            expected_kind, self.peek_token.kind
        ));
        false
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let let_token = self.current_token.clone();

        if !self.expect_peek(TokenType::Identifier) {
            return None;
        }

        let id = IdentifierExpression {
            token: self.current_token.clone(),
            value: String::from(self.current_token.literal.clone()),
        };

        if !self.expect_peek(TokenType::Assign) {
            return None;
        }

        self.next_token();

        let value = self.parse_expression(OperatorPrecedence::Lowest).unwrap();

        if self.current_token.kind != TokenType::Semicolon {
            self.next_token();
        }

        Some(Statement::Let(LetStatement {
            token: let_token,
            id,
            value: Box::new(value),
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone();

        self.next_token();

        let return_value = self.parse_expression(OperatorPrecedence::Lowest).unwrap();

        if self.current_token.kind != TokenType::Semicolon {
            self.next_token();
        }

        let statement = ReturnStatement {
            token,
            value: Some(Box::new(return_value)),
        };

        Some(Statement::Return(statement))
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

    fn parse_identifier(&mut self) -> Box<Expression> {
        let id = Expression::Identifier(IdentifierExpression {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        });

        Box::new(id)
    }

    fn parse_integer_literal(&mut self) -> Option<Box<Expression>> {
        let literal = self.current_token.literal.clone();
        let value = literal.parse::<i64>();

        match value {
            Ok(int_literal) => {
                Some(Box::new(Expression::Integer(IntegerLiteral {
                    token: self.current_token.clone(),
                    value: int_literal,
                })))
            }
            Err(e) => {
                self.errors
                    .push(format!("could not parse {:?} as integer \n\t {}", literal, e));
                None
            }
        }
    }

    fn parse_boolean(&mut self) -> Box<Expression> {
        let expression = BooleanExpression {
            token: self.current_token.clone(),
            value: self.current_token.kind == TokenType::True,
        };

        Box::new(Expression::Bool(expression))
    }

    fn parse_grouped_expression(&mut self) -> Option<Box<Expression>> {
        self.next_token();

        let expression = self.parse_expression(OperatorPrecedence::Lowest).unwrap();

        if !self.expect_peek(TokenType::RParen) {
            return None;
        }

        Some(Box::new(expression))
    }

    fn parse_block_statement(&mut self) -> Option<Box<BlockStatement>> {
        let token = self.current_token.clone();
        let mut statements: Vec<Statement> = Vec::new();

        self.next_token();

        while self.current_token.kind != TokenType::RBrace
            && self.current_token.kind != TokenType::EOF
        {
            let statement = self.parse_statement().unwrap();

            statements.push(statement);

            self.next_token();
        }

        let block_statement = BlockStatement {
            token,
            statements: Box::new(statements),
        };

        Some(Box::new(block_statement))
    }

    fn parse_if_expression(&mut self) -> Option<Box<Expression>> {
        let token = self.current_token.clone();

        if !self.expect_peek(TokenType::LParen) {
            return None;
        }

        self.next_token();

        let condition = self.parse_expression(OperatorPrecedence::Lowest).unwrap();

        if !self.expect_peek(TokenType::RParen) {
            return None;
        }

        if !self.expect_peek(TokenType::LBrace) {
            return None;
        }

        let consequence = self.parse_block_statement();

        let mut alternative = None;

        if self.peek_token.kind == TokenType::Else {
            self.next_token();

            if !self.expect_peek(TokenType::LBrace) {
                return None;
            }

            alternative = self.parse_block_statement();
        };

        let if_expr = IfExpression {
            token,
            condition: Box::new(condition),
            consequence,
            alternative,
        };

        Some(Box::new(Expression::If(if_expr)))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<IdentifierExpression>> {
        let mut identifiers: Vec<IdentifierExpression> = vec![];

        if self.peek_token.kind == TokenType::RParen {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        identifiers.push(IdentifierExpression {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        });

        while self.peek_token.kind == TokenType::Comma {
            self.next_token();
            self.next_token();
            identifiers.push(IdentifierExpression {
                token: self.current_token.clone(),
                value: self.current_token.literal.clone(),
            });
        }

        if !self.expect_peek(TokenType::RParen) {
            return None;
        }

        Some(identifiers)
    }

    fn parse_function_literal(&mut self) -> Option<Box<Expression>> {
        let token = self.current_token.clone();

        if !self.expect_peek(TokenType::LParen) {
            return None;
        }

        let parameters = self.parse_function_parameters().unwrap();

        if !self.expect_peek(TokenType::LBrace) {
            return None;
        }

        let body = self.parse_block_statement().unwrap();

        let literal = FunctionLiteral {
            token,
            parameters,
            body,
        };

        Some(Box::new(Expression::Function(literal)))
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
            right: Box::new(right),
        };

        Box::new(Expression::Infix(expression))
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Box<Expression>>> {
        let mut arguments: Vec<Box<Expression>> = vec![];

        if self.peek_token.kind == TokenType::RParen {
            self.next_token();
            return Some(arguments);
        }

        self.next_token();

        arguments.push(Box::new(
            self.parse_expression(OperatorPrecedence::Lowest).unwrap(),
        ));

        while self.peek_token.kind == TokenType::Comma {
            self.next_token();
            self.next_token();

            arguments.push(Box::new(
                self.parse_expression(OperatorPrecedence::Lowest).unwrap(),
            ));
        }

        if !self.expect_peek(TokenType::RParen) {
            return None;
        }

        Some(arguments)
    }

    fn parse_call_expression(&mut self, function: Box<Expression>) -> Box<Expression> {
        let expression = CallExpression {
            token: self.current_token.clone(),
            function,
            arguments: self.parse_call_arguments().unwrap(),
        };

        Box::new(Expression::Call(expression))
    }

    fn parse_expression(&mut self, precedence: OperatorPrecedence) -> Option<Expression> {
        let prefix = self.prefix_parse_fns.get(&self.current_token.kind);

        if prefix.is_none() {
            self.errors.push(format!(
                "no prefix parse function for {:?} found",
                &self.current_token.kind
            ));
            return None;
        }

        let mut left = match *prefix.unwrap() {
            ParseFn::Prefix(p) => p(self),
            ParseFn::PrefixOptional(p) => p(self)?,
        };

        while self.peek_token.kind != TokenType::Semicolon && precedence < self.peek_precedence() {
            // This whole infix block requires some more thought. This is the "least bad" approach I
            // could come up with to satisfy the borrow checker. It could be the case that "registering"
            // infix/prefix parse functions in the Parser constructor is a pattern that just doesn't
            // translate to rust idiomatically. RefCell was my solution. I have no idea if this
            // constitues "idiomatic" rust. It DOES at least help me avoid cloning the entire map
            // of parse functions. I need an adult. And a beer. :(
            let infix = {
                let infix_fns = self.infix_parse_fns.borrow(); // Borrow immutably
                infix_fns.get(&self.peek_token.kind).cloned() // Retrieve the function, cloned to return by value
            };

            if let Some(infix) = infix {
                self.next_token();
                left = infix(self, left);
            } else {
                return Some(*left);
            }
        }

        return Some(*left);
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone();

        let expression = match self.parse_expression(OperatorPrecedence::Lowest) {
            Some(exp) => exp,
            None => {
                return None;
            }
        };

        if self.peek_token.kind == TokenType::Semicolon {
            self.next_token();
        }

        let expr_statement = ExpressionStatement {
            token,
            expression: Box::new(expression),
        };

        Some(Statement::Expression(expr_statement))
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token.kind {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse_program(&mut self) -> Node {
        let mut statements: Vec<Statement> = vec![];

        while self.current_token.clone().kind != TokenType::EOF {
            let statement = self.parse_statement();
            if statement.is_some() {
                statements.push(statement.unwrap());
            }

            self.next_token();
        }

        Node::Program(Program { statements })
    }
}

#[cfg(test)]
mod parser_tests;
