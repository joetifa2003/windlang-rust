use super::value::Value;

#[derive(Debug)]
pub enum Opcode {
    Const(Value),
    Jmp(i32),
    JmpFalse(i32),
    Block(usize), // Var count
    Let(usize),
    Set { scope: usize, index: usize },
    Get { scope: usize, index: usize },
    Add,
    Sub,
    Mul,
    Pop,
    EndBlock,
}
