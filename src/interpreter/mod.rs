use std::{cell::RefCell, rc::Rc};

use crate::{
    error::wind_error,
    parser::ast::{AstNode, AstNodeType},
};

use self::env::{Env, Value};

pub mod env;

wind_error!(RuntimeError; UndefinedVar);

#[derive(Default)]
pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn eval_program(
        &self,
        ast: Vec<Box<AstNode>>,
        mut env: Rc<RefCell<Env>>,
    ) -> Result<Value, RuntimeError> {
        let mut value = Value::NIL;

        for node in &ast {
            value = self.eval(node, &mut env)?;
        }

        Ok(value)
    }

    fn eval(&self, node: &AstNode, env: &mut Rc<RefCell<Env>>) -> Result<Value, RuntimeError> {
        match &node.node_type {
            AstNodeType::ExpressionStmt { expression } => self.eval(expression, env),
            AstNodeType::FloatExpr { value } => Ok(Value::FLOAT(*value)),
            AstNodeType::BoolExpr { value } => Ok(Value::BOOL(*value)),
            AstNodeType::StringExpr { value } => Ok(Value::STRING(value.clone())),
            AstNodeType::IntegerExpr { value } => Ok(Value::INT(*value)),
            AstNodeType::BlockStmt { statements } => {
                let mut value = Value::NIL;

                for node in statements {
                    value = self.eval(node, env)?;
                }

                Ok(value)
            }
            AstNodeType::WhileStmt {
                condition,
                body: statements,
            } => {
                while Self::is_truthy(&self.eval(condition, env)?) {
                    self.eval(statements, env)?;
                }

                Ok(Value::NIL)
            }
            AstNodeType::LetStmt { name, value } => {
                let evaluated_val = self.eval(value, env)?;

                env.borrow_mut().let_local(name.clone(), evaluated_val);

                Ok(Value::NIL)
            }
            AstNodeType::ReturnStmt { value: _ } => todo!(),
            AstNodeType::IdentExpr { value } => env.borrow().get(value),
            AstNodeType::PrefixExpr {
                operator: _,
                right: _,
            } => todo!(),
            AstNodeType::InfixExpr {
                left,
                operator,
                right,
            } => {
                let evaluated_left = self.eval(left, env)?;
                let evaluated_right = self.eval(right, env)?;

                match (evaluated_left, evaluated_right) {
                    (Value::INT(lv), Value::INT(rv)) => match operator.as_str() {
                        "+" => Ok(Value::INT(lv + rv)),
                        "-" => Ok(Value::INT(lv - rv)),
                        "*" => Ok(Value::INT(lv * rv)),
                        "/" => Ok(Value::INT(lv / rv)),
                        "%" => Ok(Value::INT(lv % rv)),
                        "==" => Ok(Value::BOOL(lv == rv)),
                        "<=" => Ok(Value::BOOL(lv <= rv)),
                        _ => unreachable!(),
                    },
                    (Value::FLOAT(lv), Value::FLOAT(rv)) => match operator.as_str() {
                        "+" => Ok(Value::FLOAT(lv + rv)),
                        "-" => Ok(Value::FLOAT(lv - rv)),
                        "*" => Ok(Value::FLOAT(lv * rv)),
                        "/" => Ok(Value::FLOAT(lv / rv)),
                        _ => unreachable!(),
                    },
                    (Value::FLOAT(lv), Value::INT(rv)) => match operator.as_str() {
                        "+" => Ok(Value::FLOAT(lv + rv as f32)),
                        "-" => Ok(Value::FLOAT(lv - rv as f32)),
                        "*" => Ok(Value::FLOAT(lv * rv as f32)),
                        "/" => Ok(Value::FLOAT(lv / rv as f32)),
                        _ => unreachable!(),
                    },
                    (Value::INT(lv), Value::FLOAT(rv)) => match operator.as_str() {
                        "+" => Ok(Value::FLOAT(lv as f32 + rv)),
                        "-" => Ok(Value::FLOAT(lv as f32 - rv)),
                        "*" => Ok(Value::FLOAT(lv as f32 * rv)),
                        "/" => Ok(Value::FLOAT(lv as f32 / rv)),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
            AstNodeType::IfExpr {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = self.eval(condition, env)?;

                if Self::is_truthy(&condition) {
                    self.eval(then_branch, env)
                } else if let Some(else_branch) = else_branch {
                    self.eval(else_branch, env)
                } else {
                    Ok(Value::NIL)
                }
            }
            AstNodeType::ArrayExpression { values } => {
                let mut evaluated_values = vec![];

                for node in values {
                    evaluated_values.push(self.eval(node, env)?);
                }

                Ok(Value::ARRAY(evaluated_values))
            }

            AstNodeType::AssignExpr { name, value } => {
                let evaluated_value = self.eval(value, env)?;

                env.borrow_mut().assign(name, evaluated_value)
            }
            AstNodeType::Echo { value } => todo!(),
            AstNodeType::Call { func, args } => todo!(),
            AstNodeType::FunctionExpr { body, params } => todo!(),
        }
    }

    fn is_truthy(val: &Value) -> bool {
        match *val {
            Value::BOOL(val) => val,
            Value::NIL => false,
            _ => true,
        }
    }
}
