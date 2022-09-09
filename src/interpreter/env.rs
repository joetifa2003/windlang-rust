use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{RuntimeError, RuntimeErrorKind};

#[derive(Clone, Debug)]
pub enum Value {
    INT(i32),
    FLOAT(f32),
    STRING(String),
    BOOL(bool),
    ARRAY(Vec<Value>),
    NIL,
}

#[derive(Default)]
pub struct Env {
    store: HashMap<String, Value>,
    outer: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Rc<RefCell<Env>> {
        Rc::new(RefCell::new(Self::default()))
    }

    pub fn new_with_enclosing(outer: Rc<RefCell<Env>>) -> Rc<RefCell<Env>> {
        Rc::new(RefCell::new(Self {
            outer: Some(outer),
            ..Default::default()
        }))
    }

    pub fn get(&self, name: &str) -> Result<Value, RuntimeError> {
        let val = self.store.get(name);

        if let Some(val) = val {
            Ok(val.clone())
        } else if let Some(outer) = &self.outer {
            outer.borrow().get(name)
        } else {
            Err(RuntimeError::new(
                RuntimeErrorKind::UndefinedVar,
                format!("{} is not defined", name),
            ))
        }
    }

    pub fn let_local(&mut self, name: String, value: Value) {
        self.store.insert(name, value);
    }

    pub fn assign(&mut self, name: &String, value: Value) -> Result<Value, RuntimeError> {
        let val = self.store.get(name);

        if let Some(_val) = val {
            let old = self.store.insert(name.clone(), value).unwrap();

            Ok(old)
        } else if let Some(outer) = &self.outer {
            outer.borrow_mut().assign(name, value)
        } else {
            todo!()
        }
    }
}
