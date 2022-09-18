use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    error::wind_error,
    parser::ast::{AstNode, AstNodeType},
    vm::{opcode::Opcode, value::Value},
};

pub struct Compiler {
    scopes: Vec<Vec<String>>,
    built_in: HashMap<&'static str, Value>,
}

wind_error!(CompileError; UndefinedVar);

enum FindRes {
    Local { scope_offset: i32, index: usize },
    Global { index: usize },
    BuiltIn { function: Value },
}

macro_rules! built_in_func {
    ($($name:expr => $fun:expr, $params:expr);*) => {{
        let mut built_in = HashMap::new();

        $(
            built_in.insert($name, Value::BuiltInFn($fun, $params));
        )*

        built_in
    }};
}

impl Compiler {
    pub fn new() -> Compiler {
        let built_in = built_in_func!(
            "print" => |args| {
                println!("{}", args[0]);

                Value::Nil
            }, 1;
            "len" => |args| {
                match &args[0] {
                    Value::Array(v) => Value::Int(v.borrow().len() as i32),
                    _ => panic!(),
                }
            }, 1;
            "push" => |args| {
                match &args[0] {
                    Value::Array(v) => {
                        v.borrow_mut().push(args[1].clone());
                    }
                    _ => panic!(),
                }

                Value::Nil
            }, 2;
            "pop" => |args| {
                match &args[0] {
                    Value::Array(v) => {
                        v.borrow_mut().pop().unwrap()
                    }
                    _ => panic!(),
                }
            }, 1;
            "initArray" => |args| {
                let n_of_elements = match args[0].as_int() {
                    Some(v) => *v,
                    None => panic!(),
                };

                Value::Array(Rc::new(RefCell::new(Vec::with_capacity(n_of_elements as usize))))
            }, 1
        );

        Compiler {
            scopes: vec![],
            built_in,
        }
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

    fn find_in_scope(&self, input_name: String) -> Result<FindRes, CompileError> {
        for scopes_index in (0..self.scopes.len()).rev() {
            for (index, name) in self.scopes[scopes_index].iter().enumerate() {
                if input_name == *name {
                    if scopes_index == 0 {
                        return Ok(FindRes::Global { index });
                    } else {
                        return Ok(FindRes::Local {
                            scope_offset: scopes_index as i32 - self.scopes.len() as i32 + 1,
                            index,
                        });
                    }
                }
            }
        }

        let built_in = self.built_in.get(input_name.as_str());
        if let Some(function) = built_in {
            return Ok(FindRes::BuiltIn {
                function: function.clone(),
            });
        }

        Err(CompileError::new(
            CompileErrorKind::UndefinedVar,
            format!("cannot find {}", input_name),
        ))
    }

    pub fn compile_stmts(&mut self, ast: Vec<Box<AstNode>>) -> Result<Vec<Opcode>, CompileError> {
        let mut instructions = vec![];

        instructions.push(Opcode::Block(0));
        self.begin_scope();
        for node in ast {
            let mut node_instructions = self.compile(*node)?;
            instructions.append(&mut node_instructions);
        }
        let scope = self.end_scope();
        instructions.push(Opcode::EndBlock);

        instructions[0] = Opcode::Block(scope.len());

        Ok(instructions)
    }

    fn compile(&mut self, node: AstNode) -> Result<Vec<Opcode>, CompileError> {
        match node.node_type {
            AstNodeType::IntegerExpr { value } => Ok(vec![Opcode::Const(Value::Int(value))]),
            AstNodeType::FloatExpr { value } => Ok(vec![Opcode::Const(Value::Float(value))]),
            AstNodeType::BoolExpr { value } => Ok(vec![Opcode::Const(Value::Bool(value))]),
            AstNodeType::StringExpr { value } => Ok(vec![Opcode::Const(Value::String(value))]),
            AstNodeType::ExpressionStmt { expression } => {
                let mut instructions = self.compile(*expression)?;
                instructions.push(Opcode::Pop);

                Ok(instructions)
            }
            AstNodeType::BlockStmt { statements } => {
                let mut instructions = vec![];

                let mut body_instructions = self.compile_stmts(statements)?;

                instructions.append(&mut body_instructions);

                Ok(instructions)
            }
            AstNodeType::InfixExpr {
                left,
                operator,
                right,
            } => {
                let mut instructions = vec![];
                let mut left = self.compile(*left)?;
                let mut right = self.compile(*right)?;

                instructions.append(&mut right);
                instructions.append(&mut left);

                match operator.as_str() {
                    "+" => instructions.push(Opcode::Add),
                    "-" => instructions.push(Opcode::Sub),
                    "%" => instructions.push(Opcode::Mod),
                    "*" => instructions.push(Opcode::Mul),
                    "/" => instructions.push(Opcode::Div),
                    "<=" => instructions.push(Opcode::LtEq),
                    "<" => instructions.push(Opcode::Lt),
                    ">=" => instructions.push(Opcode::GtEq),
                    ">" => instructions.push(Opcode::Gt),
                    "==" => instructions.push(Opcode::Eq),
                    "!=" => instructions.push(Opcode::NotEq),
                    "&&" => instructions.push(Opcode::And),
                    "||" => instructions.push(Opcode::Or),
                    _ => panic!(),
                }

                Ok(instructions)
            }
            AstNodeType::IfExpr {
                condition,
                then_branch,
                else_branch,
            } => {
                let mut instructions = vec![];
                let mut condition_instructions = self.compile(*condition)?;
                let mut then_instructions = self.compile(*then_branch)?;
                let mut else_instructions = vec![];

                if let Some(else_branch) = else_branch {
                    else_instructions = self.compile(*else_branch)?;
                }

                instructions.append(&mut condition_instructions);
                instructions.push(Opcode::JmpFalse((then_instructions.len() + 2) as i32));
                instructions.append(&mut then_instructions);
                instructions.push(Opcode::Jmp((else_instructions.len() + 1) as i32));
                instructions.append(&mut else_instructions);

                Ok(instructions)
            }
            AstNodeType::WhileStmt { condition, body } => {
                let mut instructions = vec![];

                let mut condition_instructions = self.compile(*condition)?;
                let condition_len = condition_instructions.len() as i32;
                let mut body_instructions = self.compile(*body)?;
                let body_len = body_instructions.len() as i32;

                instructions.append(&mut condition_instructions);
                instructions.push(Opcode::JmpFalse((body_instructions.len() + 2) as i32));
                instructions.append(&mut body_instructions);
                instructions.push(Opcode::Jmp(-condition_len - body_len - 1));

                Ok(instructions)
            }
            AstNodeType::LetStmt { name, value } => {
                let mut instructions = vec![];
                let index_in_scope = self.define_in_scope(name);
                let mut value_instructions = self.compile(*value)?;

                instructions.append(&mut value_instructions);
                instructions.push(Opcode::Let(index_in_scope));

                Ok(instructions)
            }
            AstNodeType::IdentExpr { value } => match self.find_in_scope(value)? {
                FindRes::Local {
                    scope_offset,
                    index,
                } => Ok(vec![Opcode::Get {
                    scope_index: scope_offset,
                    index,
                }]),
                FindRes::Global { index } => Ok(vec![Opcode::GetGlobal { index }]),
                FindRes::BuiltIn { function } => Ok(vec![Opcode::Const(function)]),
            },
            AstNodeType::AssignExpr { name, value } => {
                let mut instructions = vec![];
                let mut value_instructions = self.compile(*value)?;
                instructions.append(&mut value_instructions);

                match self.find_in_scope(name)? {
                    FindRes::Local {
                        scope_offset,
                        index,
                    } => {
                        instructions.push(Opcode::Set {
                            scope_index: scope_offset,
                            index,
                        });
                    }
                    FindRes::Global { index } => {
                        instructions.push(Opcode::SetGlobal { index });
                    }
                    _ => panic!(),
                }

                Ok(instructions)
            }
            AstNodeType::ArrayExpression { values } => {
                let mut instructions = vec![];
                let values_len = values.len() as u32;
                for value in values.into_iter().rev() {
                    let mut val_instructions = self.compile(*value)?;
                    instructions.append(&mut val_instructions);
                }

                instructions.push(Opcode::Array(values_len));

                Ok(instructions)
            }
            AstNodeType::PrefixExpr { operator, right } => {
                let mut instructions = vec![];
                let mut value_instructions = self.compile(*right)?;

                instructions.append(&mut value_instructions);
                instructions.push(match operator.as_str() {
                    "!" => Opcode::Bang,
                    "-" => Opcode::Negate,
                    _ => unreachable!(),
                });

                Ok(instructions)
            }
            AstNodeType::Echo { value } => {
                let mut instructions = vec![];

                let mut value_instructions = self.compile(*value)?;
                instructions.append(&mut value_instructions);
                instructions.push(Opcode::Echo);

                Ok(instructions)
            }
            AstNodeType::FunctionExpr { body, params } => {
                let mut fn_instructions = vec![];
                let param_count = params.len();

                self.begin_scope();
                for param in params {
                    self.define_in_scope(param);
                }
                let mut body_instructions = self.compile(*body)?;
                self.end_scope();

                fn_instructions.push(Opcode::FnStart(param_count));
                fn_instructions.append(&mut body_instructions);
                fn_instructions.push(Opcode::FnEnd);

                Ok(vec![Opcode::Const(Value::Fn(fn_instructions, param_count))])
            }
            AstNodeType::ReturnStmt { value } => {
                let mut instructions = self.compile(*value)?;
                instructions.push(Opcode::Ret);
                Ok(instructions)
            }
            AstNodeType::Call { func, args } => {
                let mut instructions = vec![];
                let mut args_instructions = vec![];
                let args_count = args.len();

                for arg in args.into_iter().rev() {
                    let mut arg_instructions = self.compile(arg)?;
                    args_instructions.append(&mut arg_instructions);
                }

                let mut func_instructions = self.compile(*func)?;
                instructions.append(&mut args_instructions);
                instructions.append(&mut func_instructions);
                instructions.push(Opcode::Call(args_count));

                Ok(instructions)
            }
        }
    }
}
