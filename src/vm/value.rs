use std::{cell::RefCell, fmt::Display, rc::Rc};

use enum_as_inner::EnumAsInner;

use super::opcode::Opcode;

#[derive(Clone, Debug, EnumAsInner)]
pub enum Value {
    Int(i32),
    Float(f32),
    String(String),
    Bool(bool),
    Array(Rc<RefCell<Vec<Value>>>),
    Fn(Vec<Opcode>, usize),
    BuiltInFn(fn(args: Vec<Value>) -> Value, usize),
    Nil,
}

impl Value {
    pub fn value_type(&self) -> &str {
        match self {
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::String(_) => "string",
            Value::Bool(_) => "bool",
            Value::Array(_) => "array",
            Value::Fn(_, _) => "fn",
            Value::BuiltInFn(_, _) => "builtInFn",
            Value::Nil => "nil",
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Value::Int(val) => val.to_string(),
                Value::Float(val) => val.to_string(),
                Value::String(val) => val.clone(),
                Value::Bool(val) => val.to_string(),
                Value::Nil => "nil".into(),
                Value::Array(values) => {
                    let mut res = String::from("[");

                    for val in values.borrow().iter() {
                        res += format!("{},", val).as_str();
                    }

                    res += "]";

                    res
                }
                Value::Fn(instructions, _) => {
                    let mut instructions_str = String::from("{\n\t");

                    for instruction in instructions {
                        instructions_str += format!("{} \n\t", instruction).as_str();
                    }

                    instructions_str += "\r}";

                    format!("fn {}", instructions_str)
                }
                Value::BuiltInFn(_, _) => "builtInFn".into(),
            }
        )
    }
}
