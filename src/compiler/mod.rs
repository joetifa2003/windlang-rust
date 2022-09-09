use crate::{
    parser::ast::{AstNode, AstNodeType},
    vm::{opcode::Opcode, value::Value},
};

pub struct Compiler {
    scopes: Vec<Vec<String>>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler { scopes: vec![] }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(vec![]);
    }

    fn end_scope(&mut self) -> Vec<String> {
        self.scopes.pop().unwrap()
    }

    fn define_in_scope(&mut self, name: String) -> usize {
        let scope = self.scopes.last_mut().unwrap();
        scope.push(name);

        scope.len() - 1
    }

    pub fn compile(&mut self, ast: Vec<Box<AstNode>>) -> Vec<Opcode> {
        for node in ast {
            match node.node_type {
                AstNodeType::IntegerExpr { value } => {
                    return vec![Opcode::Const(Value::Int(value))];
                }
                AstNodeType::FloatExpr { value } => {
                    return vec![Opcode::Const(Value::Float(value))];
                }
                AstNodeType::BoolExpr { value } => {
                    return vec![Opcode::Const(Value::Bool(value))];
                }
                AstNodeType::StringExpr { value } => {
                    return vec![Opcode::Const(Value::String(value))];
                }
                AstNodeType::ExpressionStmt { expression } => {
                    let mut instructions = self.compile(vec![expression]);
                    instructions.push(Opcode::Pop);
                    return instructions;
                }
                AstNodeType::BlockStmt { statements } => {
                    let mut instructions = vec![];

                    self.begin_scope();
                    let mut body_instructions = self.compile(statements);
                    let scope = self.end_scope();

                    instructions.push(Opcode::Block(scope.len()));
                    instructions.append(&mut body_instructions);
                    instructions.push(Opcode::EndBlock);

                    return instructions;
                }
                AstNodeType::InfixExpr {
                    left,
                    operator,
                    right,
                } => {
                    let mut instructions = vec![];
                    let mut left = self.compile(vec![left]);
                    let mut right = self.compile(vec![right]);

                    instructions.append(&mut left);
                    instructions.append(&mut right);

                    match operator.as_str() {
                        "+" => instructions.push(Opcode::Add),
                        _ => panic!(),
                    }

                    return instructions;
                }
                AstNodeType::IfExpr {
                    condition,
                    then_branch,
                    else_branch,
                } => todo!(),
                AstNodeType::WhileStmt {
                    condition,
                    statements,
                } => todo!(),
                AstNodeType::LetStmt { name, value } => todo!(),
                AstNodeType::IdentExpr { value } => todo!(),
                AstNodeType::ReturnStmt { value } => todo!(),
                AstNodeType::PrefixExpr { operator, right } => todo!(),
                AstNodeType::ArrayExpression { values } => todo!(),
                AstNodeType::FunctionExpr { body } => todo!(),
                AstNodeType::AssignExpr { name, value } => todo!(),
            }
        }

        unreachable!()
    }
}
