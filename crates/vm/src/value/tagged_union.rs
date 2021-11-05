use crate::gc::{Ctx, Gc, Trace};

#[derive(Clone, Copy, Debug)]
pub enum TaggedValue {
    Float(f64),
    Integer(i32),
    Boolean(bool),
    String(Gc<String>),
    Undefined,
    Null,
    Empty,
}
