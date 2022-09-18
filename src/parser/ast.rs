use std::fmt;

use enum_as_inner::EnumAsInner;

#[derive(Debug, EnumAsInner, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Int,
    FLoat,
    String,
    Bool,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::FLoat => write!(f, "float"),
            Type::String => write!(f, "string"),
            Type::Bool => write!(f, "bool"),
        }
    }
}

#[derive(Debug, EnumAsInner)]
pub enum AstNodeType {
    // Statements
    ExpressionStmt {
        expression: Box<AstNode>,
    },
    BlockStmt {
        statements: Vec<Box<AstNode>>,
    },
    WhileStmt {
        condition: Box<AstNode>,
        body: Box<AstNode>,
    },
    LetStmt {
        name: String, // Identifier
        value: Box<AstNode>,
    },
    ReturnStmt {
        value: Box<AstNode>,
    },

    // Expressions
    IntegerExpr {
        value: i32,
    },
    FloatExpr {
        value: f32,
    },
    BoolExpr {
        value: bool,
    },
    StringExpr {
        value: String,
    },
    IdentExpr {
        value: String,
    },
    PrefixExpr {
        operator: String,
        right: Box<AstNode>,
    },
    InfixExpr {
        left: Box<AstNode>,
        operator: String,
        right: Box<AstNode>,
    },
    IfExpr {
        condition: Box<AstNode>,
        then_branch: Box<AstNode>,
        else_branch: Option<Box<AstNode>>,
    },
    ArrayExpression {
        values: Vec<Box<AstNode>>,
    },
    FunctionExpr {
        body: Box<AstNode>,
        params: Vec<String>,
    },
    AssignExpr {
        name: String,
        value: Box<AstNode>,
    },
    Echo {
        value: Box<AstNode>,
    },
    Call {
        func: Box<AstNode>,
        args: Vec<AstNode>,
    },
}

#[derive(Debug)]

pub struct AstNode {
    pub line: u32,
    pub node_type: AstNodeType,
}

impl AstNode {
    pub fn new(node_type: AstNodeType, line: u32) -> AstNode {
        AstNode { line, node_type }
    }
}
