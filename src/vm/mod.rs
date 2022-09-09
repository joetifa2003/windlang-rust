use self::{opcode::Opcode, value::Value};

pub mod opcode;
pub mod value;

macro_rules! number_op {
    ($stack:ident, $op:tt) => {{
        let left = $stack.pop();
        let right = $stack.pop();

        $stack.push(match (left, right) {
            (Value::Int(lv), Value::Int(rv)) => Value::Int(lv $op rv),
            (Value::Int(lv), Value::Float(rv)) => Value::Float(lv as f32 $op rv),
            (Value::Float(lv), Value::Int(rv)) => Value::Float(lv $op rv as f32),
            (Value::Float(lv), Value::Float(rv)) => Value::Float(lv $op rv),
            _ => panic!(),
        })
    }};
}

pub struct EnvironmentStack {}

pub struct VM {
    pub stack: Vec<Value>,
}

impl VM {
    pub fn new() -> VM {
        VM { stack: vec![] }
    }

    pub fn pop(&mut self) -> Value {
        let popped = self.stack.pop().unwrap();
        dbg!(&popped);
        popped
    }

    pub fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    pub fn interpret(&mut self, instructions: Vec<Opcode>) {
        let mut ip = 0;
        while ip < instructions.len() {
            let instruction = &instructions[ip];
            match instruction {
                Opcode::Const(val) => self.push(val.clone()),
                Opcode::Add => number_op!(self, +),
                Opcode::Sub => number_op!(self, -),
                Opcode::Mul => number_op!(self, *),
                Opcode::Jmp(offset) => {
                    ip += *offset as usize;
                    continue;
                }
                Opcode::JmpFalse(offset) => {
                    let operand = self.pop();

                    if is_truthy(operand) {
                        ip += *offset as usize;
                        continue;
                    }
                }
                Opcode::Pop => {
                    self.pop();
                }
                Opcode::Block(var_count) => {}
                Opcode::EndBlock => {}
                Opcode::Let(_) => todo!(),
                Opcode::Set { scope, index } => todo!(),
                Opcode::Get { scope, index } => todo!(),
            }

            ip += 1;
        }
    }
}

fn is_truthy(val: Value) -> bool {
    match val {
        Value::Bool(val) => val,
        _ => true,
    }
}
