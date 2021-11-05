use crate::gc::{Ctx, Gc, Trace};

#[derive(Clone, Copy, Debug)]
pub enum Value {
    Float(f64),
    Integer(i32),
    Boolean(bool),
    String(Gc<String>),
    Undefined,
    Null,
    Empty,
}

impl From<f64> for Value {
    fn from(v: f64) -> Value {
        Value::Float(v)
    }
}

impl From<i32> for Value {
    fn from(v: i32) -> Value {
        Value::Integer(v)
    }
}

impl From<bool> for Value {
    fn from(v: bool) -> Value {
        Value::Boolean(v)
    }
}

impl From<Gc<String>> for Value {
    fn from(v: Gc<String>) -> Value {
        Value::String(v)
    }
}

impl Value {
    pub fn undefined() -> Self {
        Value::Undefined
    }

    pub fn null() -> Self {
        Value::Null
    }

    pub fn empty() -> Self {
        Value::Empty
    }
}

unsafe impl Trace for Value {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }
    fn trace(&self, ctx: Ctx) {
        match self {
            Self::Integer(_)
            | Self::Float(_)
            | Self::Boolean(_)
            | Self::Null
            | Self::Undefined
            | Self::Empty => {}
            Self::String(x) => ctx.mark(*x),
        }
    }
}
