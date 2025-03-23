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

    fn read_string(&mut self) -> &str {
        let position = (self.position.clone() + 1) as usize;

        loop {
            self.read_char();

            if self.ch == '"' || self.ch == '\0' {
                break;
            }
        }

        return &self.input[position..self.position as usize];
    }

    pub fn next_token(&mut self) -> Token {
        use TokenType::*;

        self.skip_whitespace();

        let token: Token = match self.ch {
            '=' => {
                if self.peek() == '=' {
                    self.read_char();
                    self.read_char();
                    return Token::new(Equal, String::from("=="));
                }
                Token::new(Assign, self.ch.into())
            }
            '+' => Token::new(Plus, self.ch.into()),
            '-' => Token::new(Minus, self.ch.into()),
            '!' => {
                if self.peek() == '=' {
                    self.read_char();
                    self.read_char();
                    return Token::new(NotEqual, String::from("!="));
                }
                Token::new(Bang, self.ch.into())
            }
            '*' => Token::new(Asterisk, self.ch.into()),
            '/' => Token::new(Slash, self.ch.into()),
            '<' => Token::new(LT, self.ch.into()),
            '>' => Token::new(GT, self.ch.into()),
            '(' => Token::new(LParen, self.ch.into()),
            ')' => Token::new(RParen, self.ch.into()),
            '{' => Token::new(LBrace, self.ch.into()),
            '}' => Token::new(RBrace, self.ch.into()),
            '[' => Token::new(LBracket, self.ch.into()),
            ']' => Token::new(RBracket, self.ch.into()),
            ',' => Token::new(Comma, self.ch.into()),
            ';' => Token::new(Semicolon, self.ch.into()),
            '"' => Token::new(Str, self.read_string().into()),
            '\0' => {
                if self.position == 0 {
                    // If the we are at the beggining of the input,
                    // we obviously are not at the EOF, so we eat the
                    // next char and keep lexing.
                    self.read_char();
                    return self.next_token();
                }
                Token::new(EOF, String::new())
            }
            _ => {
                if self.ch.is_alphabetic() || self.ch == '_' {
                    let id = self.read_identifier();
                    return Token::new(lookup_identifier(id), id.into());
                } else if self.ch.is_digit(10) {
                    return Token::new(Int, self.read_number().into());
                }
                panic!("Failed to lex illegal character: {}", self.ch);
            }
        };

        self.read_char();
        return token;
    }
}

#[cfg(test)]
mod lexer_tests;
