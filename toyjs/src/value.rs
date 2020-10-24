use runtime::value::JSValue;

#[derive(Clone, Debug)]
pub enum Value {
    Null,
    Undefined,
    Int(i32),
    Float(f64),
    String(String),
    Bool(bool),
    Object,
}

impl Value {
    pub(crate) unsafe fn from_js_value(value: JSValue) -> Self {
        if value.is_null() {
            return Value::Null;
        }
        if value.is_undefined() {
            return Value::Undefined;
        }
        if value.is_string() {
            return Value::String((*value.into_string()).clone());
        }
        if value.is_int() {
            return Value::Int(value.into_int());
        }
        if value.is_float() {
            return Value::Float(value.into_float());
        }
        if value.is_object() {
            return Value::Object;
        }
        if value.is_bool() {
            return Value::Bool(value.into_bool());
        }
        todo!()
    }
}
