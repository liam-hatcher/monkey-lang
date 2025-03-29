#[derive(PartialEq, Debug, Clone, Hash, Eq)]
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
    NotEqual,
    Str,
    LBracket,
    RBracket,
    Colon
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Token {
    pub kind: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(kind: TokenType, literal: String) -> Self {
        Self {
            kind,
            literal: literal,
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Self {
            kind: TokenType::EOF,
            literal: String::new()
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
        _ => TokenType::Identifier,
    }
}
