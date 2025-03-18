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
type InfixParseFn = fn(&mut Parser, Box<Expression>) -> Box<Expression>;

pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Token,
    peek_token: Token,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    // This seems like a hack, but i'm a n00b so ¯\_(ツ)_/¯
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
        use TokenType::*;

        let mut parser: Parser<'a> = Self {
            lexer,
            current_token: Token::default(),
            peek_token: Token::default(),
            // errors: Vec::default(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: RefCell::new(HashMap::new()),
        };

        parser.register_prefix(Identifier, |p| p.parse_identifier());
        parser.register_prefix(Int, |p| p.parse_integer_literal());
        parser.register_prefix(Bang, |p| p.parse_prefix_expression());
        parser.register_prefix(Minus, |p| p.parse_prefix_expression());
        parser.register_prefix(True, |p| p.parse_boolean());
        parser.register_prefix(False, |p| p.parse_boolean());
        parser.register_prefix(LParen, |p| p.parse_grouped_expression());
        parser.register_prefix(If, |p| p.parse_if_expression());
        parser.register_prefix(Function, |p| p.parse_function_literal());

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

    fn register_prefix(&mut self, token_type: TokenType, func: PrefixParseFn) {
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

        let id = IdentifierExpression {
            token: self.current_token.clone(),
            value: String::from(self.current_token.literal.clone()),
        };

        if let Err(e) = self.expect_peek(TokenType::Assign) {
            return Err(e);
        }

        self.next_token();

        let value = self.parse_expression(OperatorPrecedence::Lowest).unwrap();

        
        if self.current_token.kind != TokenType::Semicolon {
            self.next_token();
        }

        Ok(Statement::Let(LetStatement {
            token: let_token,
            id,
            value: Box::new(value)
        }))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.current_token.clone();

        self.next_token();

        let return_value = self.parse_expression(OperatorPrecedence::Lowest).unwrap();

        if self.current_token.kind != TokenType::Semicolon {
            self.next_token();
        }

        let statement = Return {
            token,
            value: Some(Box::new(return_value)),
        };

        Ok(Statement::Return(statement))
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

    fn parse_boolean(&mut self) -> Box<Expression> {
        let expression = BooleanExpression {
            token: self.current_token.clone(),
            value: self.current_token.kind == TokenType::True,
        };

        Box::new(Expression::Bool(expression))
    }

    fn parse_grouped_expression(&mut self) -> Box<Expression> {
        self.next_token();

        let expression = self.parse_expression(OperatorPrecedence::Lowest).unwrap();

        if let Err(e) = self.expect_peek(TokenType::RParen) {
            panic!("Failed to parse grouped expression:\n {:?}", e); // This should never happen
        }

        Box::new(expression)
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

    fn parse_if_expression(&mut self) -> Box<Expression> {
        let token = self.current_token.clone();

        if let Err(e) = self.expect_peek(TokenType::LParen) {
            panic!("Failed to parse if expression:\n {:?}", e); // This should never happen
        }

        self.next_token();

        let condition = self.parse_expression(OperatorPrecedence::Lowest).unwrap();

        if let Err(e) = self.expect_peek(TokenType::RParen) {
            panic!("Failed to parse if expression:\n {:?}", e);
        }

        if let Err(e) = self.expect_peek(TokenType::LBrace) {
            panic!("Failed to parse if expression:\n {:?}", e);
        }

        let consequence = self.parse_block_statement();

        let mut alternative = None;

        if self.peek_token.kind == TokenType::Else {
            self.next_token();

            if let Err(e) = self.expect_peek(TokenType::LBrace) {
                panic!("Failed to parse if expression:\n {:?}", e);
            }

            alternative = self.parse_block_statement();
        };

        let if_expr = IfExpression {
            token,
            condition: Box::new(condition),
            consequence,
            alternative,
        };

        Box::new(Expression::If(if_expr))
    }

    fn parse_function_parameters(&mut self) -> Vec<IdentifierExpression> {
        let mut identifiers: Vec<IdentifierExpression> = vec![];

        if self.peek_token.kind == TokenType::RParen {
            self.next_token();
            return identifiers;
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

        if let Err(e) = self.expect_peek(TokenType::RParen) {
            panic!("Failed to parse function parameters: \n {:?}", e);
        }

        identifiers
    }

    fn parse_function_literal(&mut self) -> Box<Expression> {
        let token = self.current_token.clone();

        if let Err(e) = self.expect_peek(TokenType::LParen) {
            panic!("Failed to parse function literal expression:\n {:?}", e);
        }

        let parameters = self.parse_function_parameters();

        if let Err(e) = self.expect_peek(TokenType::LBrace) {
            panic!("Failed to parse function literal expression:\n {:?}", e);
        }

        let body = self.parse_block_statement().unwrap();

        let literal = FunctionLiteral {
            token,
            parameters,
            body,
        };

        Box::new(Expression::Function(literal))
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

    fn parse_call_arguments(&mut self) -> Vec<Box<Expression>> {
        let mut arguments: Vec<Box<Expression>> = vec![];

        if self.peek_token.kind == TokenType::RParen {
            self.next_token();
            return arguments;
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

        if let Err(e) = self.expect_peek(TokenType::RParen) {
            panic!("Failed to parse call expression arguments:\n {:?}", e);
        }

        arguments
    }

    fn parse_call_expression(&mut self, function: Box<Expression>) -> Box<Expression> {
        let expression = CallExpression {
            token: self.current_token.clone(),
            function,
            arguments: self.parse_call_arguments(),
        };

        Box::new(Expression::Call(expression))
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
                return Ok(*left);
            }
        }

        return Ok(*left);
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
