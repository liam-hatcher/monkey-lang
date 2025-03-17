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
pub struct BooleanExpression {
    pub token: Token,
    pub value: bool,
}

impl BooleanExpression {
    pub fn to_string(&self) -> String {
        String::from(&self.token.literal)
    }
}

#[derive(Debug)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,

    // these option types might be unnecessary
    pub consequence: Option<Box<BlockStatement>>,
    pub alternative: Option<Box<BlockStatement>>,
}

impl IfExpression {
    pub fn to_string(&self) -> String {
        let condition = &*self.condition.to_string();
        let consequence = self.consequence.as_ref().unwrap().to_string();
        
        if let Some(alt) = self.alternative.as_ref() {
            return format!("if({:?} {:?}else {:?}", condition, consequence, alt.to_string());
        }

        format!("if({:?} {:?}", condition, consequence)
    }
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    Integer(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Bool(BooleanExpression),
    If(IfExpression),
}

impl Expression {
    pub fn to_string(&self) -> String {
        match self {
            Expression::Identifier(id) => id.to_string(),
            Expression::Integer(i) => i.to_string(),
            Expression::Prefix(pe) => pe.to_string(),
            Expression::Infix(ie) => ie.to_string(),
            Expression::Bool(b) => b.to_string(),
            Expression::If(ie) => ie.to_string(),
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
pub struct BlockStatement {
    pub token: Token,
    pub statements: Box<Vec<Statement>>,
}

impl BlockStatement {
    pub fn to_string(&self) -> String {
        let mut output = String::new();
        for s in &*self.statements {
            output += &s.to_string()
        }
        output
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
    Block(BlockStatement),
}

impl Statement {
    pub fn to_string(&self) -> String {
        match self {
            Statement::Let(s) => s.to_string(),
            Statement::Expression(ex) => ex.to_string(),
            Statement::Return(r) => r.to_string(),
            Statement::Block(bs) => bs.to_string(),
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
