#[derive(Clone, Debug)]
pub enum Value {
    Int(i32),
    Float(f32),
    String(String),
    Bool(bool),
}
