use crate::token::*;

pub struct Lexer {
    input: String,
    position: u32,
    read_position: u32,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Self {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
        }
    }

    // The purpose of readChar is to give us the next character
    // and advance our position in the input string.
    fn read_char(&mut self) {
        if self.read_position as usize >= self.input.len() {
            self.ch = '\0';
        } else {
            let char_at_read_pos = self.input.chars().nth(self.read_position as usize);
            self.ch = char_at_read_pos.unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek(&self) -> char {
        if self.read_position as usize > self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.read_position as usize).unwrap()
        }
    }

    fn read_number(&mut self) -> &str {
        let position = self.position.clone() as usize;
        while self.ch.is_digit(10) {
            self.read_char();
        }
        return &self.input[position..self.position as usize];
    }

    fn read_identifier(&mut self) -> &str {
        let position = self.position.clone() as usize;
        while self.ch.is_alphabetic() {
            self.read_char();
        }
        return &self.input[position..self.position as usize];
    }

    fn skip_whitespace(&mut self) {
        match self.ch {
            ' ' | '\t' | '\n' | '\r' => self.read_char(),
            _ => return,
        }
        self.skip_whitespace();
    }

    

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token: Token = match self.ch {
            '=' => {
                if self.peek() == '=' {
                    self.read_char();
                    self.read_char();
                    return Token::new(TokenType::Equal, String::from("=="));
                }
                Token::new(TokenType::Assign, self.ch.into())
            },
            '+' => Token::new(TokenType::Plus, self.ch.into()),
            '-' => Token::new(TokenType::Minus, self.ch.into()),
            '!' => {
                if self.peek() == '=' {
                    self.read_char();
                    self.read_char();
                    return Token::new(TokenType::NotEqual, String::from("!="));
                }
                Token::new(TokenType::Bang, self.ch.into())
            },
            '*' => Token::new(TokenType::Asterisk, self.ch.into()),
            '/' => Token::new(TokenType::Slash, self.ch.into()),
            '<' => Token::new(TokenType::LT, self.ch.into()),
            '>' => Token::new(TokenType::GT, self.ch.into()),
            '(' => Token::new(TokenType::LParen, self.ch.into()),
            ')' => Token::new(TokenType::RParen, self.ch.into()),
            '{' => Token::new(TokenType::LBrace, self.ch.into()),
            '}' => Token::new(TokenType::RBrace, self.ch.into()),
            ',' => Token::new(TokenType::Comma, self.ch.into()),
            ';' => Token::new(TokenType::Semicolon, self.ch.into()),
            '\0' => {
                if self.position == 0 {
                    // If the we are at the beggining of the input,
                    // we obviously are not at the EOF, so we eat the
                    // next char and keep lexing.
                    self.read_char();
                    return self.next_token();
                }
                Token::new(TokenType::EOF, String::new())
            }
            _ => {
                if self.ch.is_alphabetic() || self.ch == '_' {
                    let id = self.read_identifier();
                    return Token::new(lookup_identifier(id), id.into());
                } else if self.ch.is_digit(10) {
                    return Token::new(TokenType::Int, self.read_number().into());
                }
                return Token::new(TokenType::Illegal, self.ch.into());
            }
        };

        self.read_char();
        return token;
    }
}

#[cfg(test)]
mod lexer_tests;