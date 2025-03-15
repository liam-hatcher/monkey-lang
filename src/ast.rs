use crate::token::Token;

// I might not need this Node enum at all
pub enum Node<'a> {
    Statement(Statement<'a>),
    Expression(Expression<'a>),
}


pub struct Let<'a> {
    token: Token,
    id: Identifier<'a>,
    value: Expression<'a>,
}

pub struct Identifier<'a> {
    token: Token,
    value: &'a str
}

pub enum Statement<'a> {
    Let(Let<'a>)
}

pub enum Expression<'a> {
    Identifier(Identifier<'a>)
}

pub struct Program<'a> {
    statements: Vec<Statement<'a>>
}

