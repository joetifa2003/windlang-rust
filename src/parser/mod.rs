use crate::{
    error::wind_error,
    lexer::token::{Token, TokenType},
};

use self::ast::{AstNode, AstNodeType};

pub mod ast;

wind_error!(ParseError; InvalidExpression, UnexpectedToken);

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Assign,
    Or,
    And,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Postfix,
    Highest,
}

type PrefixFn = Box<dyn Fn(&mut Parser) -> Result<Box<AstNode>, ParseError>>;
type InfixFn = Box<dyn Fn(&mut Parser, Box<AstNode>) -> Result<Box<AstNode>, ParseError>>;

pub struct Parser {
    tokens: Vec<Token>,
    current_idx: usize,
    peek_idx: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            current_idx: 0,
            peek_idx: 1,
        }
    }

    pub fn parse_program(&mut self) -> Result<Vec<Box<AstNode>>, ParseError> {
        let mut program: Vec<Box<AstNode>> = vec![];

        while self.current_token().tt != TokenType::Eof {
            let stmt = self.parse_statement()?;
            program.push(stmt);
        }

        Ok(program)
    }

    fn next_token(&mut self) {
        self.current_idx = self.peek_idx;
        self.peek_idx += 1;
    }

    fn current_token(&self) -> &Token {
        &self.tokens[self.current_idx]
    }

    fn peek_token(&self) -> &Token {
        &self.tokens[self.peek_idx]
    }

    fn expect_current(&mut self, tt: TokenType) -> Result<(), ParseError> {
        if !self.current_token_is(tt) {
            return Err(ParseError::new(
                ParseErrorKind::UnexpectedToken,
                format!("unexpected token {}", self.current_token().literal),
            ));
        }

        self.next_token();
        Ok(())
    }

    fn current_token_is(&self, tt: TokenType) -> bool {
        self.current_token().tt == tt
    }

    fn peek_token_is(&self, tt: TokenType) -> bool {
        self.peek_token().tt == tt
    }

    fn parse_statement(&mut self) -> Result<Box<AstNode>, ParseError> {
        match self.current_token().tt {
            TokenType::While => self.parse_while(),
            TokenType::Let => self.parse_let(),
            TokenType::Return => self.parse_return(),
            TokenType::LBrace => self.parse_block_stmt(),
            TokenType::If => self.parse_if_stmt(),
            TokenType::Echo => self.parse_echo_stmt(),
            TokenType::For => self.parse_for_stmt(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_block_stmt(&mut self) -> Result<Box<AstNode>, ParseError> {
        let line = self.current_token().line;
        self.expect_current(TokenType::LBrace)?;

        let mut statements = vec![];

        while !self.current_token_is(TokenType::RBrace) && !self.current_token_is(TokenType::Eof) {
            statements.push(self.parse_statement()?);
        }

        self.expect_current(TokenType::RBrace)?;

        Ok(Box::new(AstNode::new(
            AstNodeType::BlockStmt { statements },
            line,
        )))
    }

    fn parse_for_stmt(&mut self) -> Result<Box<AstNode>, ParseError> {
        let line = self.current_token().line;
        self.next_token(); // for

        self.expect_current(TokenType::LParen)?;
        let initializer = self.parse_statement()?;
        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect_current(TokenType::SemiColon)?;
        let increment = self.parse_statement()?;
        self.expect_current(TokenType::RParen)?;
        let body = self.parse_statement()?;

        Ok(Box::new(AstNode::new(
            AstNodeType::ForStmt {
                initializer,
                condition,
                body,
                increment,
            },
            line,
        )))
    }

    fn parse_while(&mut self) -> Result<Box<AstNode>, ParseError> {
        let line = self.current_token().line;
        self.next_token(); // while

        self.expect_current(TokenType::LParen)?;

        let expr = self.parse_expression(Precedence::Lowest)?;

        self.expect_current(TokenType::RParen)?;

        let stmt = self.parse_statement()?;

        Ok(Box::new(AstNode {
            line,
            node_type: AstNodeType::WhileStmt {
                condition: expr,
                body: stmt,
            },
        }))
    }

    fn parse_let(&mut self) -> Result<Box<AstNode>, ParseError> {
        let line = self.current_token().line;
        self.next_token();

        let expr_name = self.current_token().literal.clone();
        self.expect_current(TokenType::Ident)?;

        self.expect_current(TokenType::Assign)?;
        let expr_val = self.parse_expression(Precedence::Lowest)?;
        self.expect_current(TokenType::SemiColon)?;

        Ok(Box::new(AstNode::new(
            AstNodeType::LetStmt {
                name: expr_name,
                value: expr_val,
            },
            line,
        )))
    }

    fn parse_return(&mut self) -> Result<Box<AstNode>, ParseError> {
        let line = self.current_token().line;
        self.next_token(); // return

        let expr = self.parse_expression(Precedence::Lowest)?;

        self.expect_current(TokenType::SemiColon)?;

        Ok(Box::new(AstNode::new(
            AstNodeType::ReturnStmt { value: expr },
            line,
        )))
    }

    fn parse_expression_statement(&mut self) -> Result<Box<AstNode>, ParseError> {
        let expr_stmt_line = self.current_token().line;
        let expr = self.parse_expression(Precedence::Lowest)?;

        let expr_stmt = AstNode::new(
            AstNodeType::ExpressionStmt { expression: expr },
            expr_stmt_line,
        );

        if self.current_token_is(TokenType::SemiColon) {
            self.next_token();
        }

        Ok(Box::new(expr_stmt))
    }

    fn parse_int(parser: &mut Parser) -> Result<Box<AstNode>, ParseError> {
        let integer = AstNode::new(
            AstNodeType::IntegerExpr {
                value: parser.current_token().literal.parse().unwrap(),
            },
            parser.current_token().line,
        );

        parser.next_token();

        Ok(Box::new(integer))
    }

    fn parse_float(parser: &mut Parser) -> Result<Box<AstNode>, ParseError> {
        let float = AstNode::new(
            AstNodeType::FloatExpr {
                value: parser.current_token().literal.parse().unwrap(),
            },
            parser.current_token().line,
        );

        parser.next_token();

        Ok(Box::new(float))
    }

    fn parse_group(parser: &mut Parser) -> Result<Box<AstNode>, ParseError> {
        parser.next_token();

        let expr = parser.parse_expression(Precedence::Lowest)?;

        parser.expect_current(TokenType::RParen)?;

        Ok(expr)
    }

    fn parse_bool(parser: &mut Parser) -> Result<Box<AstNode>, ParseError> {
        let bool = AstNode::new(
            AstNodeType::BoolExpr {
                value: parser.current_token_is(TokenType::True),
            },
            parser.current_token().line,
        );

        parser.next_token();

        Ok(Box::new(bool))
    }

    fn parse_string(parser: &mut Parser) -> Result<Box<AstNode>, ParseError> {
        let string = AstNode::new(
            AstNodeType::StringExpr {
                value: parser.current_token().literal.clone(),
            },
            parser.current_token().line,
        );

        parser.next_token();

        Ok(Box::new(string))
    }

    fn parse_ident(parser: &mut Parser) -> Result<Box<AstNode>, ParseError> {
        let ident = AstNode::new(
            AstNodeType::IdentExpr {
                value: parser.current_token().literal.clone(),
            },
            parser.current_token().line,
        );

        parser.next_token();

        Ok(Box::new(ident))
    }

    fn parse_infix(parser: &mut Parser, left: Box<AstNode>) -> Result<Box<AstNode>, ParseError> {
        let operator = parser.current_token().literal.clone();
        let line = parser.current_token().line;
        let precedence = parser.current_precedence()?;

        parser.next_token();

        let right = parser.parse_expression(precedence)?;

        Ok(Box::new(AstNode::new(
            AstNodeType::InfixExpr {
                left,
                right,
                operator,
            },
            line,
        )))
    }

    fn parse_assign(parser: &mut Parser, left: Box<AstNode>) -> Result<Box<AstNode>, ParseError> {
        let line = parser.current_token().line;
        parser.next_token(); // =

        let value = parser.parse_expression(Precedence::Lowest)?;

        Ok(Box::new(AstNode::new(
            AstNodeType::AssignExpr {
                name: left.node_type.as_ident_expr().unwrap().clone(),
                value,
            },
            line,
        )))
    }

    fn parse_call(parser: &mut Parser, left: Box<AstNode>) -> Result<Box<AstNode>, ParseError> {
        let line = parser.current_token().line;
        let mut args = vec![];

        parser.next_token(); // (
        if !parser.current_token_is(TokenType::RParen) {
            while parser.peek_token_is(TokenType::Comma) {
                let arg = parser.parse_expression(Precedence::Lowest)?;
                args.push(*arg);
                parser.next_token(); // ,
            }

            let last_param = parser.parse_expression(Precedence::Lowest)?;
            args.push(*last_param);
        }
        parser.expect_current(TokenType::RParen)?;

        Ok(Box::new(AstNode {
            line,
            node_type: AstNodeType::Call { func: left, args },
        }))
    }

    fn parse_if_stmt(&mut self) -> Result<Box<AstNode>, ParseError> {
        let line = self.current_token().line;
        self.next_token(); // if keyword

        self.expect_current(TokenType::LParen)?;

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_current(TokenType::RParen)?;

        let then_statement = self.parse_statement()?;
        let mut else_statement: Option<Box<AstNode>> = None;

        if self.current_token_is(TokenType::Else) {
            self.next_token();

            else_statement = Some(self.parse_statement()?);
        }

        Ok(Box::new(AstNode::new(
            AstNodeType::IfExpr {
                condition,
                then_branch: then_statement,
                else_branch: else_statement,
            },
            line,
        )))
    }

    fn parse_echo_stmt(&mut self) -> Result<Box<AstNode>, ParseError> {
        let line = self.current_token().line;
        self.next_token(); // echo

        let value = self.parse_expression(Precedence::Lowest)?;

        self.expect_current(TokenType::SemiColon)?;

        Ok(Box::new(AstNode {
            line,
            node_type: AstNodeType::Echo { value },
        }))
    }

    fn parse_prefix(parser: &mut Parser) -> Result<Box<AstNode>, ParseError> {
        let line = parser.current_token().line;
        let operator = parser.current_token().literal.clone();

        parser.next_token();

        let expr = parser.parse_expression(Precedence::Prefix)?;

        Ok(Box::new(AstNode::new(
            AstNodeType::PrefixExpr {
                operator,
                right: expr,
            },
            line,
        )))
    }

    fn parse_array(parser: &mut Parser) -> Result<Box<AstNode>, ParseError> {
        let line = parser.current_token().line;
        parser.next_token(); // [

        let mut expressions = vec![];
        if !parser.current_token_is(TokenType::RBracket) {
            expressions.push(parser.parse_expression(Precedence::Lowest)?);

            while parser.current_token_is(TokenType::Comma) {
                parser.next_token(); // ,
                expressions.push(parser.parse_expression(Precedence::Lowest)?);
            }
        }

        parser.expect_current(TokenType::RBracket)?;

        Ok(Box::new(AstNode::new(
            AstNodeType::ArrayExpression {
                values: expressions,
            },
            line,
        )))
    }

    fn parse_fn(parser: &mut Parser) -> Result<Box<AstNode>, ParseError> {
        let line = parser.current_token().line;
        let mut params = vec![];

        parser.next_token(); // fn
        parser.expect_current(TokenType::LParen)?;
        if !parser.current_token_is(TokenType::RParen) {
            while parser.peek_token_is(TokenType::Comma) {
                let param = parser.current_token().literal.clone();
                parser.expect_current(TokenType::Ident)?;
                params.push(param);
                parser.next_token(); // ,
            }

            let last_param = parser.current_token().literal.clone();
            parser.expect_current(TokenType::Ident)?;
            params.push(last_param);
        }

        parser.expect_current(TokenType::RParen)?;

        let body = parser.parse_block_stmt()?;

        Ok(Box::new(AstNode::new(
            AstNodeType::FunctionExpr { body, params },
            line,
        )))
    }

    fn get_prefix_parse_fn(&self) -> Result<PrefixFn, ParseError> {
        match self.current_token().tt {
            TokenType::Int => Ok(Box::new(Self::parse_int)),
            TokenType::Float => Ok(Box::new(Self::parse_float)),
            TokenType::LParen => Ok(Box::new(Self::parse_group)),
            TokenType::True | TokenType::False => Ok(Box::new(Self::parse_bool)),
            TokenType::String => Ok(Box::new(Self::parse_string)),
            TokenType::Ident => Ok(Box::new(Self::parse_ident)),
            TokenType::Bang | TokenType::Minus => Ok(Box::new(Self::parse_prefix)),
            TokenType::LBracket => Ok(Box::new(Self::parse_array)),
            TokenType::Function => Ok(Box::new(Self::parse_fn)),
            _ => Err(ParseError::new(
                ParseErrorKind::InvalidExpression,
                format!("expected expression got {}", self.current_token().literal),
            )),
        }
    }

    fn get_infix_parse_fn(&self) -> Option<InfixFn> {
        match self.current_token().tt {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Asterisk
            | TokenType::Slash
            | TokenType::Equal
            | TokenType::NotEqual
            | TokenType::LessThan
            | TokenType::LessThanEq
            | TokenType::GreaterThan
            | TokenType::GreaterThanEq
            | TokenType::Modulo
            | TokenType::And
            | TokenType::Or => Some(Box::new(Parser::parse_infix)),

            TokenType::Assign => Some(Box::new(Parser::parse_assign)),
            TokenType::LParen => Some(Box::new(Parser::parse_call)),

            _ => None,
        }
    }

    fn current_precedence(&self) -> Result<Precedence, ParseError> {
        self.get_precedence(self.current_token().tt)
    }

    fn get_precedence(&self, tt: TokenType) -> Result<Precedence, ParseError> {
        match tt {
            TokenType::Equal | TokenType::NotEqual => Ok(Precedence::Equals),
            TokenType::Assign => Ok(Precedence::Assign),

            TokenType::LessThan
            | TokenType::GreaterThan
            | TokenType::LessThanEq
            | TokenType::GreaterThanEq => Ok(Precedence::LessGreater),

            TokenType::Plus | TokenType::Minus => Ok(Precedence::Sum),
            TokenType::Asterisk | TokenType::Modulo | TokenType::Slash => Ok(Precedence::Product),
            TokenType::PlusPlus | TokenType::MinusMinus => Ok(Precedence::Postfix),
            TokenType::And => Ok(Precedence::And),
            TokenType::Or => Ok(Precedence::Or),
            TokenType::LParen | TokenType::Dot => Ok(Precedence::Highest),

            _ => Ok(Precedence::Lowest),
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Box<AstNode>, ParseError> {
        let prefix = self.get_prefix_parse_fn()?;
        let mut left_expr = prefix(self)?;

        while !self.current_token_is(TokenType::SemiColon)
            && !self.current_token_is(TokenType::Eof)
            && self.current_precedence()? > precedence
        {
            if let Some(infix) = self.get_infix_parse_fn() {
                left_expr = infix(self, left_expr)?;
            } else {
                return Ok(left_expr);
            }
        }

        Ok(left_expr)
    }
}
