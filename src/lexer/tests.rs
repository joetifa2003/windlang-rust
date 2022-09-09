use super::{
    token::{Token, TokenType},
    Lexer,
};

fn assert_token(token: &Token, tt: TokenType, literal: &str) {
    assert!(token.tt == tt);
    assert!(token.literal == literal);
}

#[test]
fn test_symbols() {
    let code = String::from("; + - * / % = == () {} [] && || ++ -- != ! < <= > >= . .. :");
    let mut lexer = Lexer::new(code);

    assert_token(&lexer.next_token().unwrap(), TokenType::SemiColon, ";");
    assert_token(&lexer.next_token().unwrap(), TokenType::Plus, "+");
    assert_token(&lexer.next_token().unwrap(), TokenType::Minus, "-");
    assert_token(&lexer.next_token().unwrap(), TokenType::Asterisk, "*");
    assert_token(&lexer.next_token().unwrap(), TokenType::Slash, "/");
    assert_token(&lexer.next_token().unwrap(), TokenType::Modulo, "%");
    assert_token(&lexer.next_token().unwrap(), TokenType::Assign, "=");
    assert_token(&lexer.next_token().unwrap(), TokenType::Equal, "==");
    assert_token(&lexer.next_token().unwrap(), TokenType::LParen, "(");
    assert_token(&lexer.next_token().unwrap(), TokenType::RParen, ")");
    assert_token(&lexer.next_token().unwrap(), TokenType::LBrace, "{");
    assert_token(&lexer.next_token().unwrap(), TokenType::RBrace, "}");
    assert_token(&lexer.next_token().unwrap(), TokenType::LBracket, "[");
    assert_token(&lexer.next_token().unwrap(), TokenType::RBracket, "]");
    assert_token(&lexer.next_token().unwrap(), TokenType::And, "&&");
    assert_token(&lexer.next_token().unwrap(), TokenType::Or, "||");
    assert_token(&lexer.next_token().unwrap(), TokenType::PlusPlus, "++");
    assert_token(&lexer.next_token().unwrap(), TokenType::MinusMinus, "--");
    assert_token(&lexer.next_token().unwrap(), TokenType::NotEqual, "!=");
    assert_token(&lexer.next_token().unwrap(), TokenType::Bang, "!");
    assert_token(&lexer.next_token().unwrap(), TokenType::LessThan, "<");
    assert_token(&lexer.next_token().unwrap(), TokenType::LessThanEq, "<=");
    assert_token(&lexer.next_token().unwrap(), TokenType::GreaterThan, ">");
    assert_token(&lexer.next_token().unwrap(), TokenType::GreaterThanEq, ">=");
    assert_token(&lexer.next_token().unwrap(), TokenType::Dot, ".");
    assert_token(&lexer.next_token().unwrap(), TokenType::DotDot, "..");
    assert_token(&lexer.next_token().unwrap(), TokenType::Colon, ":");
}

#[test]
fn test_strings() {
    let code = String::from("\"Hello World\"");
    let mut lexer = Lexer::new(code);

    assert_token(
        &lexer.next_token().unwrap(),
        TokenType::String,
        "Hello World",
    );
}

#[test]
fn test_numbers() {
    let code = String::from("1 10 12 1.5 1.69");
    let mut lexer = Lexer::new(code);

    assert_token(&lexer.next_token().unwrap(), TokenType::Int, "1");
    assert_token(&lexer.next_token().unwrap(), TokenType::Int, "10");
    assert_token(&lexer.next_token().unwrap(), TokenType::Int, "12");
    assert_token(&lexer.next_token().unwrap(), TokenType::Float, "1.5");
    assert_token(&lexer.next_token().unwrap(), TokenType::Float, "1.69");
}

#[test]
fn test_ident() {
    let code = String::from("x x2");
    let mut lexer = Lexer::new(code);

    assert_token(&lexer.next_token().unwrap(), TokenType::Ident, "x");
    assert_token(&lexer.next_token().unwrap(), TokenType::Ident, "x2");
}

#[test]
fn test_keywords() {
    let code = String::from(
        "fn let const true false if else return for include while nil as break continue",
    );
    let mut lexer = Lexer::new(code);

    assert_token(&lexer.next_token().unwrap(), TokenType::Function, "fn");
    assert_token(&lexer.next_token().unwrap(), TokenType::Let, "let");
    assert_token(&lexer.next_token().unwrap(), TokenType::Const, "const");
    assert_token(&lexer.next_token().unwrap(), TokenType::True, "true");
    assert_token(&lexer.next_token().unwrap(), TokenType::False, "false");
    assert_token(&lexer.next_token().unwrap(), TokenType::If, "if");
    assert_token(&lexer.next_token().unwrap(), TokenType::Else, "else");
    assert_token(&lexer.next_token().unwrap(), TokenType::Return, "return");
    assert_token(&lexer.next_token().unwrap(), TokenType::For, "for");
    assert_token(&lexer.next_token().unwrap(), TokenType::Include, "include");
    assert_token(&lexer.next_token().unwrap(), TokenType::While, "while");
    assert_token(&lexer.next_token().unwrap(), TokenType::Nil, "nil");
    assert_token(&lexer.next_token().unwrap(), TokenType::As, "as");
    assert_token(&lexer.next_token().unwrap(), TokenType::Break, "break");
    assert_token(
        &lexer.next_token().unwrap(),
        TokenType::Continue,
        "continue",
    );
}
