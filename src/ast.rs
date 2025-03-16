use crate::token::Token;

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn to_string(&self) -> String {
        self.value.clone()
    }
}

#[derive(Debug)]
pub struct Let {
    pub token: Token,
    pub id: Identifier,
    // pub value: Expression, // TODO
}

impl Let {
    pub fn to_string(&self) -> String {
        let literal = &self.token.literal;
        let name = &self.id.value;

        format!("{literal} {name} = [PLACEHOLDER];")
    }
}

#[derive(Debug)]
pub struct Return {
    pub token: Token,
    // pub value: Expression // todo
}

impl Return {
    pub fn to_string(&self) -> String {
        let literal = &self.token.literal;
        // let return_value = &self.value.to_string();

        // if return_value { // todo }

        format!("{literal} [PLACEHOLDER];")
    }
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl IntegerLiteral {
    pub fn to_string(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl PrefixExpression {
    pub fn to_string(&self) -> String {
        let op = &self.operator;
        let right = &*self.right.to_string();

        format!("({op}{right})")
    }
}

#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl InfixExpression {
    pub fn to_string(&self) -> String {
        let left = &*self.left.to_string();
        let op = &self.operator;
        let right = &*self.right.to_string();
        format!("({left} {op} {right})")
    }
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    Integer(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
}

impl Expression {
    pub fn to_string(&self) -> String {
        match self {
            Expression::Identifier(id) => id.to_string(),
            Expression::Integer(i) => i.to_string(),
            Expression::Prefix(pe) => pe.to_string(),
            Expression::Infix(ie) => ie.to_string(),
        }
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Box<Expression>,
}

impl ExpressionStatement {
    pub fn to_string(&self) -> String {
        String::from(&*self.expression.to_string())
    }
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

impl Statement {
    pub fn to_string(&self) -> String {
        match self {
            Self::Let(s) => s.to_string(),
            Self::Expression(ex) => ex.to_string(),
            Self::Return(r) => r.to_string(),
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn to_string(&self) -> String {
        let mut output = String::new();
        for s in &self.statements {
            output += &s.to_string();
        }

        output
    }
}
