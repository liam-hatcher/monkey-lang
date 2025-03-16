use crate::token::Token;

// I might not need this Node enum at all
pub enum Node {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug)]
pub struct Let {
    pub token: Token,
    pub id: Identifier,
    // pub value: Expression, // TODO
}

#[derive(Debug)]
pub struct Return {
    pub token: Token,
    // pub value: Expression // todo
}

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String
}

#[derive(Debug)]
pub enum Statement {
    Let(Let),
    Return(Return)
}

pub enum Expression {
    Identifier(Identifier)
}

pub struct Program {
    pub statements: Vec<Statement>
}

