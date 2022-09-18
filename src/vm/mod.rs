use std::{cell::RefCell, rc::Rc};

use self::{opcode::Opcode, value::Value};
use crate::error::wind_error;
pub mod opcode;
pub mod value;

wind_error!(RuntimeError; InvalidOperation);

macro_rules! binary_op_error {
    ($left:tt $op:tt $right:tt) => {{
        return Err(RuntimeError::new(
            RuntimeErrorKind::InvalidOperation,
            format!(
                "cannot evaluate {} {} {}",
                $left.value_type(),
                stringify!($op),
                $right.value_type()
            ),
        ));
    }};
}

macro_rules! number_op {
    ($stack:ident, $op:tt) => {{
        let left = $stack.pop();
        let right = $stack.pop();

        $stack.push(match (&left, &right) {
            (Value::Int(lv), Value::Int(rv)) => Value::Int(*lv $op *rv),
            (Value::Int(lv), Value::Float(rv)) => Value::Float(*lv as f32 $op *rv),
            (Value::Float(lv), Value::Int(rv)) => Value::Float(*lv $op *rv as f32),
            (Value::Float(lv), Value::Float(rv)) => Value::Float(*lv $op *rv),
            _ => binary_op_error!((&left) $op (&right)),
        })
    }};
}

macro_rules! number_op_bool {
    ($stack:ident, $op:tt) => {{
        let left = $stack.pop();
        let right = $stack.pop();

        $stack.push(match (&left, &right) {
            (Value::Int(lv), Value::Int(rv)) => Value::Bool(*lv $op *rv),
            (Value::Int(lv), Value::Float(rv)) => Value::Bool((*lv as f32) $op *rv),
            (Value::Float(lv), Value::Int(rv)) => Value::Bool(*lv $op *rv as f32),
            (Value::Float(lv), Value::Float(rv)) => Value::Bool(*lv $op *rv),
            _ => binary_op_error!((&left) $op (&right)),
        })
    }};
}

const STACK_SIZE: usize = 512;

pub struct Environment {
    store: Vec<Value>,
}

pub struct EnvironmentStack {
    stack: Vec<Environment>,
}

impl EnvironmentStack {
    fn new() -> EnvironmentStack {
        EnvironmentStack { stack: vec![] }
    }

    #[inline(always)]
    fn begin_environment(&mut self, var_count: usize) {
        self.stack.push(Environment {
            store: vec![Value::Nil; var_count],
        })
    }

    #[inline(always)]
    fn end_environment(&mut self) {
        self.stack.pop();
    }

    #[inline(always)]
    fn current_env_mut(&mut self) -> &mut Environment {
        self.stack.last_mut().unwrap()
    }

    #[inline(always)]
    fn define(&mut self, index: usize, value: Value) {
        let env = self.current_env_mut();
        env.store[index] = value;
    }

    #[inline(always)]
    fn index_from_relative(&self, input: i32) -> usize {
        (self.stack.len() as i32 + input - 1) as usize
    }

    #[inline(always)]
    fn get(&self, scope_index: i32, index: usize) -> Value {
        self.stack[self.index_from_relative(scope_index)].store[index].clone()
    }

    #[inline(always)]
    fn set(&mut self, scope_index: i32, index: usize, value: Value) {
        let scope_index = self.index_from_relative(scope_index);
        self.stack[scope_index].store[index] = value
    }
}

pub struct Frame {
    instructions: Vec<Opcode>,
    ip: i32,
    num_of_blocks: i32,
}

impl Frame {
    #[inline(always)]
    fn new(instructions: Vec<Opcode>) -> Frame {
        Frame {
            instructions,
            ip: 0,
            num_of_blocks: 0,
        }
    }
}

pub struct VM {
    pub stack: Vec<Value>,
    env_stack: EnvironmentStack,
    frames: Vec<Frame>,
}

impl VM {
    pub fn new(instructions: Vec<Opcode>) -> VM {
        Self {
            stack: Vec::with_capacity(STACK_SIZE),
            env_stack: EnvironmentStack::new(),
            frames: vec![Frame::new(instructions)],
        }
    }

    #[inline(always)]
    fn pop(&mut self) -> Value {
        let popped = self.stack.pop().unwrap();
        // dbg!(&popped);
        popped
    }

    #[inline(always)]
    fn stack_top(&self) -> &Value {
        self.stack.last().unwrap()
    }

    #[inline(always)]
    fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    #[inline(always)]
    fn define(&mut self, index: usize, val: Value) {
        self.env_stack.define(index, val);
    }

    #[inline(always)]
    fn begin_environment(&mut self, var_count: usize) {
        self.current_frame_mut().num_of_blocks += 1;
        self.env_stack.begin_environment(var_count)
    }

    #[inline(always)]
    fn end_environment(&mut self) {
        self.current_frame_mut().num_of_blocks -= 1;
        self.env_stack.end_environment()
    }

    #[inline(always)]
    fn get(&mut self, scope_index: i32, index: usize) -> Value {
        self.env_stack.get(scope_index, index)
    }

    #[inline(always)]
    fn set(&mut self, scope_index: i32, index: usize, value: Value) {
        self.env_stack.set(scope_index, index, value)
    }

    #[inline(always)]
    fn current_frame(&self) -> &Frame {
        self.frames.last().unwrap()
    }

    #[inline(always)]
    fn current_frame_mut(&mut self) -> &mut Frame {
        self.frames.last_mut().unwrap()
    }

    #[inline(always)]
    fn current_frame_instruction(&self) -> Opcode {
        let curr_frame = self.current_frame();

        curr_frame.instructions[curr_frame.ip as usize].clone()
    }

    #[inline(always)]
    fn push_frame(&mut self, instructions: Vec<Opcode>) {
        self.frames.push(Frame::new(instructions));
    }

    #[inline(always)]
    fn pop_frame(&mut self) {
        self.frames.pop();
    }

    pub fn interpret(&mut self) -> Result<(), RuntimeError> {
        while self.current_frame().ip < self.current_frame().instructions.len() as i32 {
            let instruction = self.current_frame_instruction();
            match instruction {
                Opcode::Const(val) => self.push(val),
                Opcode::Add => number_op!(self, +),
                Opcode::Sub => number_op!(self, -),
                Opcode::Mul => number_op!(self, *),
                Opcode::Div => number_op!(self, /),
                Opcode::Mod => number_op!(self, %),
                Opcode::LtEq => number_op_bool!(self, <=),
                Opcode::Lt => number_op_bool!(self, <),
                Opcode::GtEq => number_op_bool!(self, >=),
                Opcode::Gt => number_op_bool!(self, >),
                Opcode::Eq => {
                    let left = self.pop();
                    let right = self.pop();

                    self.push(Value::Bool(is_equal(left, right)))
                }
                Opcode::NotEq => {
                    let left = self.pop();
                    let right = self.pop();

                    self.push(Value::Bool(!is_equal(left, right)))
                }
                Opcode::Jmp(offset) => {
                    self.current_frame_mut().ip += offset;
                    continue;
                }
                Opcode::JmpFalse(offset) => {
                    let operand = self.pop();

                    if !is_truthy(operand) {
                        self.current_frame_mut().ip += offset;
                        continue;
                    }
                }
                Opcode::Pop => {
                    self.pop();
                }
                Opcode::Block(var_count) => {
                    self.begin_environment(var_count);
                }
                Opcode::EndBlock => {
                    self.end_environment();
                }
                Opcode::Let(index) => {
                    let value = self.pop();

                    self.define(index, value);
                }
                Opcode::Get {
                    scope_index: scope,
                    index,
                } => {
                    let val = self.get(scope, index);

                    self.push(val);
                }
                Opcode::Set {
                    scope_index: scope,
                    index,
                } => {
                    let value = self.stack_top();

                    self.set(scope, index, value.clone());
                }
                Opcode::Array(size) => {
                    let mut values = vec![];

                    for _ in 0..size {
                        values.push(self.pop());
                    }

                    self.push(Value::Array(Rc::new(RefCell::new(values))));
                }
                Opcode::Negate => {
                    let operand = self.pop();

                    match operand {
                        Value::Int(val) => self.push(Value::Int(-val)),
                        Value::Float(val) => self.push(Value::Float(-val)),
                        _ => panic!(),
                    }
                }
                Opcode::Bang => {
                    let operand = self.pop();

                    self.push(Value::Bool(!is_truthy(operand)));
                }
                Opcode::And => {
                    let left = self.pop();
                    let right = self.pop();

                    match (left, right) {
                        (Value::Bool(lv), Value::Bool(rv)) => self.push(Value::Bool(lv && rv)),
                        _ => panic!(),
                    }
                }
                Opcode::Or => {
                    let left = self.pop();
                    let right = self.pop();

                    match (left, right) {
                        (Value::Bool(lv), Value::Bool(rv)) => self.push(Value::Bool(lv || rv)),
                        _ => panic!(),
                    }
                }
                Opcode::Echo => {
                    let operand = self.pop();

                    println!("{}", operand);
                }
                Opcode::Call(arg_count) => {
                    let operand = self.pop();

                    match operand {
                        Value::Fn(instructions, param_count) => {
                            if arg_count != param_count {
                                panic!()
                            }

                            self.push_frame(instructions);
                            continue;
                        }
                        Value::BuiltInFn(f, param_count) => {
                            if arg_count != param_count {
                                panic!()
                            }

                            let mut args = Vec::with_capacity(param_count);
                            for _ in 0..param_count {
                                args.push(self.pop());
                            }

                            self.push(f(args))
                        }
                        _ => panic!("cannot call {}", operand.value_type()),
                    }
                }
                Opcode::Ret => {
                    let operand = self.pop();

                    for _ in 0..self.current_frame().num_of_blocks {
                        self.end_environment();
                    }

                    self.pop_frame();
                    self.push(operand);
                }
                Opcode::FnStart(param_count) => {
                    self.begin_environment(param_count);

                    for i in 0..param_count {
                        let arg = self.pop();
                        self.define(i, arg);
                    }
                }
                Opcode::FnEnd => {
                    self.end_environment();
                    self.pop_frame();
                    self.push(Value::Nil);
                }
                Opcode::GetGlobal { index } => {
                    let val = self.env_stack.stack[0].store[index].clone();

                    self.push(val);
                }
                Opcode::SetGlobal { index } => {
                    let value = self.stack_top();

                    self.env_stack.stack[0].store[index] = value.clone();
                }
            }

            self.current_frame_mut().ip += 1;
        }

        Ok(())
    }
}

#[inline(always)]
fn is_truthy(val: Value) -> bool {
    match val {
        Value::Bool(val) => val,
        Value::Nil => false,
        _ => true,
    }
}

#[inline(always)]
fn is_equal(left: Value, right: Value) -> bool {
    match (left, right) {
        (Value::Int(lv), Value::Int(rv)) => (lv == rv),
        (Value::Int(lv), Value::Float(rv)) => ((lv as f32) == (rv)),
        (Value::Float(lv), Value::Int(rv)) => (lv == rv as f32),
        (Value::Float(lv), Value::Float(rv)) => (lv == rv),
        (Value::Bool(lv), Value::Bool(rv)) => (lv == rv),
        (Value::String(lv), Value::String(rv)) => lv == rv,
        (Value::Nil, Value::Nil) => true,
        _ => false,
    }
}
