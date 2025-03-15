#[derive(PartialEq, Debug, Clone)]
pub enum TokenType {
    Illegal,
    EOF,
    Identifier,
    Int,
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LT,
    GT,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    Equal,
    NotEqual
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(kind: TokenType, literal: String) -> Self {
        Self {
            kind,
            literal: literal.into(),
        }
    }
}

pub fn lookup_identifier(identifier: &str) -> TokenType {
    match identifier {
        "fn" => TokenType::Function,
        "let" => TokenType::Let,
        "true" => TokenType::True,
        "false" => TokenType::False,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "return" => TokenType::Return,
        _ => TokenType::Identifier
    }
}
