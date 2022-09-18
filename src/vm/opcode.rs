use std::fmt::Display;

use super::value::Value;

#[derive(Debug, Clone)]
pub enum Opcode {
    Const(Value),                           // Pushes a const value to the stack
    Jmp(i32),                               // Jump anyway
    JmpFalse(i32),                          // Jump to offset if false
    Block(usize),                           // Begins a block with var count
    Let(usize),                             // Defines a var with index
    Array(u32),                             // Constructs and array with len
    Set { scope_index: i32, index: usize }, // Get variable by index of scope and var
    Get { scope_index: i32, index: usize }, // Set variable by index of scope and var
    GetGlobal { index: usize },
    SetGlobal { index: usize },
    FnStart(usize),
    FnEnd,
    Add,      // +
    Sub,      // -
    Mul,      // *
    Div,      // /
    Mod,      // %
    Pop,      // Pops a value from the stack
    EndBlock, // Ends the block and cleans up vars
    LtEq,     // <=
    Lt,       // <
    GtEq,     // >=
    Gt,       // >
    Eq,       // ==
    NotEq,    // !=
    Negate,   // -
    Bang,     // !
    And,      // &&
    Or,       // ||
    Echo,
    Call(usize),
    Ret,
}

impl Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Opcode::Const(val) => format!("const {}", val),
                Opcode::Jmp(offset) => format!("jmp {}", offset),
                Opcode::JmpFalse(offset) => format!("jmpf {}", offset),
                Opcode::Block(var_count) => format!("block {}", var_count),
                Opcode::Let(idx) => format!("let {}", idx),
                Opcode::Array(len) => format!("array {}", len),
                Opcode::Set { scope_index, index } => format!("set {} {}", scope_index, index),
                Opcode::Get { scope_index, index } => format!("get {} {}", scope_index, index),
                Opcode::Add => "add".into(),
                Opcode::Sub => "sub".into(),
                Opcode::Mul => "mul".into(),
                Opcode::Pop => "pop".into(),
                Opcode::EndBlock => "endblock".into(),
                Opcode::LtEq => "lt".into(),
                Opcode::Mod => "lteq".into(),
                Opcode::Eq => "eq".into(),
                Opcode::Negate => "neg".into(),
                Opcode::Bang => "bang".into(),
                Opcode::Div => "div".into(),
                Opcode::Lt => "<".into(),
                Opcode::GtEq => ">=".into(),
                Opcode::Gt => ">".into(),
                Opcode::And => "and".into(),
                Opcode::Or => "or".into(),
                Opcode::NotEq => "neq".into(),
                Opcode::Echo => "echo".into(),
                Opcode::Call(_) => "call".into(),
                Opcode::Ret => "ret".into(),
                Opcode::FnStart(var_count) => format!("fnstart {}", var_count),
                Opcode::FnEnd => "fnend".into(),
                Opcode::GetGlobal { index } => format!("getglobal {}", index),
                Opcode::SetGlobal { index } => format!("setglobal {}", index),
            }
        )
    }
}
