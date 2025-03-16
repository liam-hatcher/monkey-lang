use crate::token::Token;

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
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
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    Integer(IntegerLiteral),
    PrefixExpression(PrefixExpression),
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Box<Expression>,
}

#[derive(Debug)]
pub enum Statement {
    Let(Let),
    Return(Return),

    // The most basic kind of expression is a statement
    // e.g.
    // x + 10;
    // foobar;
    Expression(ExpressionStatement),
}

pub struct Program {
    pub statements: Vec<Statement>,
}
