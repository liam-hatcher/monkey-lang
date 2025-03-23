use crate::token::Token;

pub trait ASTNode {
    fn to_string(&self) -> String;
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl ASTNode for StringLiteral {
    fn to_string(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Box<Expression>>,
}

impl ASTNode for ArrayLiteral {
    fn to_string(&self) -> String {
        let elements = self
            .elements
            .iter()
            .map(|el| el.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        format!("[{}]", elements)
    }
}

#[derive(Debug, Clone)]
pub struct IdentifierExpression {
    pub token: Token,
    pub value: String,
}

impl ASTNode for IdentifierExpression {
    fn to_string(&self) -> String {
        self.value.clone()
    }
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub token: Token,
    pub id: IdentifierExpression,
    pub value: Box<Expression>,
}

impl ASTNode for LetStatement {
    fn to_string(&self) -> String {
        let literal = &self.token.literal;
        let name = &self.id.value;
        let value = &*self.value.to_string();

        format!("{literal} {name} = {value}")
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub value: Option<Box<Expression>>,
}

impl ASTNode for ReturnStatement {
    fn to_string(&self) -> String {
        let literal = &self.token.literal;
        if self.value.is_some() {
            let return_value = &self.value.clone().unwrap();
            return format!("{literal} {};", return_value.to_string());
        } else {
            return format!("{literal};");
        };
    }
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl ASTNode for IntegerLiteral {
    fn to_string(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl ASTNode for PrefixExpression {
    fn to_string(&self) -> String {
        let op = &self.operator;
        let right = &*self.right.to_string();

        format!("({op}{right})")
    }
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl ASTNode for InfixExpression {
    fn to_string(&self) -> String {
        let left = &*self.left.to_string();
        let op = &self.operator;
        let right = &*self.right.to_string();
        format!("({left} {op} {right})")
    }
}

#[derive(Debug, Clone)]
pub struct BooleanExpression {
    pub token: Token,
    pub value: bool,
}

impl ASTNode for BooleanExpression {
    fn to_string(&self) -> String {
        String::from(&self.token.literal)
    }
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,

    // these option types might be unnecessary
    pub consequence: Option<Box<BlockStatement>>,
    pub alternative: Option<Box<BlockStatement>>,
}

impl ASTNode for IfExpression {
    fn to_string(&self) -> String {
        let condition = &*self.condition.to_string();
        let consequence = self.consequence.as_ref().unwrap().to_string();

        if let Some(alt) = self.alternative.as_ref() {
            return format!(
                "if({:?} {:?}else {:?}",
                condition,
                consequence,
                alt.to_string()
            );
        }

        format!("if({:?} {:?}", condition, consequence)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<IdentifierExpression>,
    pub body: Box<BlockStatement>,
}

impl ASTNode for FunctionLiteral {
    fn to_string(&self) -> String {
        let params = self
            .parameters
            .iter()
            .map(|i| i.value.as_str())
            .collect::<Vec<&str>>()
            .join(", ");

        format!(
            "{}({}){}",
            self.token.literal,
            params,
            self.body.to_string()
        )
    }
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Box<Expression>>,
}

impl ASTNode for CallExpression {
    fn to_string(&self) -> String {
        let arguments = self
            .arguments
            .iter()
            .map(|i| String::from(&i.to_string()))
            .collect::<Vec<String>>()
            .join(", ");

        format!("{}({})", self.function.to_string(), arguments)
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(IdentifierExpression),
    Integer(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Bool(BooleanExpression),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
    String(StringLiteral),
    Array(ArrayLiteral)
}

impl ASTNode for Expression {
    // Is this impl even necessary? seems dumb
    fn to_string(&self) -> String {
        use Expression::*;
        match self {
            Identifier(id) => id.to_string(),
            Integer(i) => i.to_string(),
            Prefix(pe) => pe.to_string(),
            Infix(ie) => ie.to_string(),
            Bool(b) => b.to_string(),
            If(ie) => ie.to_string(),
            Function(f) => f.to_string(),
            Call(c) => c.to_string(),
            String(s) => s.to_string(),
            Array(a) => a.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Box<Expression>,
}

impl ASTNode for ExpressionStatement {
    fn to_string(&self) -> String {
        String::from(&*self.expression.to_string())
    }
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Box<Vec<Statement>>,
}

impl ASTNode for BlockStatement {
    fn to_string(&self) -> String {
        let mut output = String::new();
        for s in &*self.statements {
            output += &s.to_string()
        }
        output
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    // The most basic kind of expression is a statement
    // e.g.
    // x + 10;
    // foobar;
    Expression(ExpressionStatement),
    Block(BlockStatement),
}

impl ASTNode for Statement {
    fn to_string(&self) -> String {
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

pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}
