use self::token::{Token, TokenType};
use crate::error::wind_error;

wind_error!(LexerError; IllegalToken);

#[cfg(test)]
mod tests;
pub mod token;

pub struct Lexer {
    code: String,
    position: usize,
    read_position: usize,
    current_char: char,
    line: u32,
}

impl Lexer {
    pub fn new(code: String) -> Lexer {
        let mut lexer = Lexer {
            code,
            position: 0,
            read_position: 0,
            current_char: '\0',
            line: 0,
        };

        lexer.read_char();

        lexer
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens: Vec<Token> = vec![];

        loop {
            let tok = self.next_token()?;
            let tok_type = tok.tt;
            tokens.push(tok);

            if tok_type == TokenType::Eof {
                break;
            }
        }

        Ok(tokens)
    }

    fn read_char(&mut self) {
        if self.read_position >= self.code.len() {
            self.current_char = '\0';
        } else {
            self.current_char = self.code.chars().nth(self.read_position).unwrap();
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn skip_whitespace(&mut self) {
        while self.current_char == '\n'
            || self.current_char == '\t'
            || self.current_char == '\r'
            || self.current_char == ' '
        {
            if self.current_char == '\n' {
                self.line += 1;
            }

            self.read_char();
        }
    }

    fn peak_char(&self) -> char {
        if self.read_position >= self.code.len() {
            '\0'
        } else {
            self.code.chars().nth(self.read_position).unwrap()
        }
    }

    fn next_token(&mut self) -> Result<Token, LexerError> {
        let tok: Token;
        self.skip_whitespace();

        match self.current_char {
            ';' => {
                tok = Token::new(TokenType::SemiColon, ";".into(), self.line);
            }
            ':' => {
                tok = Token::new(TokenType::Colon, ":".into(), self.line);
            }
            '(' => tok = Token::new(TokenType::LParen, "(".into(), self.line),
            ')' => tok = Token::new(TokenType::RParen, ")".into(), self.line),
            ',' => tok = Token::new(TokenType::Comma, ",".into(), self.line),
            '%' => tok = Token::new(TokenType::Modulo, "%".into(), self.line),
            '*' => tok = Token::new(TokenType::Asterisk, "*".into(), self.line),
            '{' => tok = Token::new(TokenType::LBrace, "{".into(), self.line),
            '}' => tok = Token::new(TokenType::RBrace, "}".into(), self.line),
            '[' => tok = Token::new(TokenType::LBracket, "[".into(), self.line),
            ']' => tok = Token::new(TokenType::RBracket, "]".into(), self.line),
            '"' => return Ok(self.read_string()),
            '-' => {
                if self.peak_char() == '-' {
                    self.read_char();
                    tok = Token::new(TokenType::MinusMinus, "--".into(), self.line);
                } else {
                    tok = Token::new(TokenType::Minus, "-".into(), self.line);
                }
            }
            '=' => {
                if self.peak_char() == '=' {
                    self.read_char();
                    tok = Token::new(TokenType::Equal, "==".into(), self.line)
                } else {
                    tok = Token::new(TokenType::Assign, "=".into(), self.line)
                }
            }
            '&' => {
                if self.peak_char() == '&' {
                    self.read_char();
                    tok = Token::new(TokenType::And, "&&".into(), self.line)
                } else {
                    return Err(LexerError::new(
                        LexerErrorKind::IllegalToken,
                        format!("illegal token {}{}", self.current_char, self.peak_char()),
                    ));
                }
            }
            '|' => {
                if self.peak_char() == '|' {
                    self.read_char();
                    tok = Token::new(TokenType::Or, "||".into(), self.line)
                } else {
                    return Err(LexerError::new(
                        LexerErrorKind::IllegalToken,
                        format!("illegal token {}{}", self.current_char, self.peak_char()),
                    ));
                }
            }
            '+' => {
                if self.peak_char() == '+' {
                    self.read_char();
                    tok = Token::new(TokenType::PlusPlus, "++".into(), self.line);
                } else {
                    tok = Token::new(TokenType::Plus, "+".into(), self.line);
                }
            }
            '!' => {
                if self.peak_char() == '=' {
                    self.read_char();
                    tok = Token::new(TokenType::NotEqual, "!=".into(), self.line);
                } else {
                    tok = Token::new(TokenType::Bang, "!".into(), self.line);
                }
            }
            '/' => {
                if self.peak_char() == '/' {
                    self.read_char();

                    while self.current_char != '\n' && self.current_char != '\0' {
                        self.read_char();
                    }

                    return self.next_token();
                } else {
                    tok = Token::new(TokenType::Slash, "/".into(), self.line);
                }
            }
            '<' => {
                if self.peak_char() == '=' {
                    self.read_char();
                    tok = Token::new(TokenType::LessThanEq, "<=".into(), self.line);
                } else {
                    tok = Token::new(TokenType::LessThan, "<".into(), self.line);
                }
            }
            '>' => {
                if self.peak_char() == '=' {
                    self.read_char();
                    tok = Token::new(TokenType::GreaterThanEq, ">=".into(), self.line);
                } else {
                    tok = Token::new(TokenType::GreaterThan, ">".into(), self.line);
                }
            }
            '.' => {
                if self.peak_char() == '.' {
                    self.read_char();
                    tok = Token::new(TokenType::DotDot, "..".into(), self.line);
                } else {
                    tok = Token::new(TokenType::Dot, ".".into(), self.line);
                }
            }
            '\0' => {
                tok = Token::new(TokenType::Eof, "".into(), self.line);
            }
            _ => {
                if self.current_char.is_alphabetic() {
                    return Ok(self.read_identifier());
                } else if self.current_char.is_ascii_digit() {
                    return Ok(self.read_number());
                } else {
                    return Err(LexerError::new(
                        LexerErrorKind::IllegalToken,
                        format!("illegal token {}", self.current_char),
                    ));
                }
            }
        }

        self.read_char();

        Ok(tok)
    }

    fn read_string(&mut self) -> Token {
        let mut literal = String::new();

        self.read_char(); // "

        while self.current_char != '"' && self.current_char != '\0' {
            literal.push(self.current_char);
            self.read_char();
        }

        self.read_char(); // "

        Token::new(TokenType::String, literal, self.line)
    }

    fn read_identifier(&mut self) -> Token {
        let mut ident = String::new();

        while self.current_char.is_alphanumeric() {
            ident.push(self.current_char);
            self.read_char();
        }

        Token::new(self.get_ident_type(&ident), ident, self.line)
    }

    fn get_ident_type(&self, ident_literal: &str) -> TokenType {
        match ident_literal {
            "fn" => TokenType::Function,
            "let" => TokenType::Let,
            "const" => TokenType::Const,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "return" => TokenType::Return,
            "for" => TokenType::For,
            "include" => TokenType::Include,
            "while" => TokenType::While,
            "nil" => TokenType::Nil,
            "as" => TokenType::As,
            "break" => TokenType::Break,
            "continue" => TokenType::Continue,
            "echo" => TokenType::Echo,
            _ => TokenType::Ident,
        }
    }

    fn read_number(&mut self) -> Token {
        let mut dot_count = 0;
        let mut literal = String::new();
        let mut token_type = TokenType::Int;

        while self.current_char.is_ascii_digit() || self.current_char == '.' {
            literal.push(self.current_char);
            self.read_char();

            if self.current_char == '.' {
                dot_count += 1;
            }
        }

        if dot_count == 1 {
            token_type = TokenType::Float;
        }

        Token::new(token_type, literal, self.line)
    }
}
