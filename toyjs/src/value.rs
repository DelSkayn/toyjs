use runtime::value::{self, JSValue};

#[derive(Clone, Debug)]
pub enum Value {
    Null,
    Undefined,
    Int(i32),
    Float(f64),
    String,
    Object,
}

impl Value {
    pub(crate) unsafe fn from_js_value(value: JSValue) -> Self {
        match value.tag() {
            value::TAG_NULL => Value::Null,
            value::TAG_UNDEFINED => Value::Undefined,
            value::TAG_STRING => Value::String,
            value::TAG_INT => Value::Int(value.into_int()),
            value::TAG_OBJECT => Value::Object,
            value::TAG_AVAILABLE_5 => panic!(),
            _ => Value::Float(value.into_float()),
        }
    }
}
