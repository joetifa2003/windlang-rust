#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    Eof,

    // Identifiers + literals
    Ident, // add, foobar, x, y, ...
    Int,   // 1343456
    String,
    Float,

    Comma,
    Colon,
    Dot,
    SemiColon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Modulo,        // %
    LessThan,      // <
    GreaterThan,   // >
    LessThanEq,    // <=
    GreaterThanEq, // >=
    And,           // &&
    Or,            // ||
    Equal,         // ==
    NotEqual,      // !=
    PlusPlus,
    MinusMinus,
    DotDot,

    // Keywords
    Function,
    Let,
    Const,
    True,
    False,
    If,
    Else,
    Return,
    For,
    Include,
    While,
    Nil,
    As,
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub tt: TokenType,
    pub literal: String,
    pub line: u32,
}

impl Token {
    pub fn new(tt: TokenType, literal: String, line: u32) -> Token {
        Token { tt, literal, line }
    }
}
