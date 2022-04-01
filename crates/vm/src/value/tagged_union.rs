use std::{fmt, mem};

use crate::{
    gc::{Ctx, Gc, Trace},
    Object,
};

#[derive(Clone, Copy)]
pub enum Value {
    Float(f64),
    Integer(i32),
    Boolean(bool),
    String(Gc<String>),
    Object(Gc<Object>),
    Undefined,
    Null,
    Empty,
}

impl Value {
    /// Returns wether two values have the same data type.
    #[inline]
    pub fn same_type(self, other: Value) -> bool {
        mem::discriminant(&self) == mem::discriminant(&other)
    }

    /// Is this value a gc allocated value.
    #[inline]
    pub fn requires_gc(self) -> bool {
        self.is_object() || self.is_string()
    }

    /// Is this value a boolean.
    #[inline]
    pub fn is_empty(self) -> bool {
        match self {
            Value::Empty => true,
            _ => false,
        }
    }

    /// Is this value a boolean.
    #[inline]
    pub fn is_bool(self) -> bool {
        match self {
            Value::Boolean(_) => true,
            _ => false,
        }
    }

    /// Is this value the value false.
    #[inline]
    pub fn is_false(self) -> bool {
        match self {
            Value::Boolean(false) => true,
            _ => false,
        }
    }

    /// Is this value the value true.
    #[inline]
    pub fn is_true(self) -> bool {
        match self {
            Value::Boolean(true) => true,
            _ => false,
        }
    }

    /// Is this value the value null.
    #[inline]
    pub fn is_null(self) -> bool {
        match self {
            Value::Null => true,
            _ => false,
        }
    }

    /// Is this value the value null.
    #[inline]
    pub fn is_undefined(self) -> bool {
        match self {
            Value::Undefined => true,
            _ => false,
        }
    }

    /// Is this value null like.
    #[inline]
    pub fn is_nullish(self) -> bool {
        match self {
            Value::Undefined | Value::Null => true,
            _ => false,
        }
    }

    /// Is this value a number type.
    #[inline]
    pub fn is_number(self) -> bool {
        match self {
            Value::Integer(_) | Value::Float(_) => true,
            _ => false,
        }
    }

    /// Is this value a number integer.
    #[inline]
    pub fn is_int(self) -> bool {
        match self {
            Value::Integer(_) => true,
            _ => false,
        }
    }

    /// Is this value a number float.
    #[inline]
    pub fn is_float(self) -> bool {
        match self {
            Value::Float(_) => true,
            _ => false,
        }
    }

    /// Is this value a object.
    #[inline]
    pub fn is_object(self) -> bool {
        match self {
            Value::Object(_) => true,
            _ => false,
        }
    }

    /// Is this value a string.
    #[inline]
    pub fn is_string(self) -> bool {
        match self {
            Value::String(_) => true,
            _ => false,
        }
    }

    /// Is this value a function.
    #[inline]
    pub unsafe fn is_function(self) -> bool {
        self.is_object() && self.unsafe_cast_object().is_function()
    }

    /// Create a new value containing the undefined javascript value.
    #[inline]
    pub const fn undefined() -> Self {
        Value::Undefined
    }

    /// Create a new value containing the null javascript value.
    #[inline]
    pub const fn null() -> Self {
        Value::Null
    }

    /// Create a new value which is empty.
    /// This value is vm internal only and should not be exposed outside of the vm.
    #[inline]
    pub const fn empty() -> Self {
        Value::Empty
    }

    #[inline]
    pub const fn nan() -> Self {
        Value::Float(f64::NAN)
    }

    /// Convert the value to `bool`
    ///
    /// # Safety
    ///
    /// Will return arbitrary values if `is_bool` returns false
    #[inline]
    pub fn cast_bool(self) -> bool {
        match self {
            Value::Boolean(x) => x,
            _ => panic!(),
        }
    }

    /// Convert the value to `i32`
    ///
    /// # Safety
    ///
    /// Will return arbitrary values if `is_int` returns false
    #[inline]
    pub fn cast_int(self) -> i32 {
        match self {
            Value::Integer(x) => x,
            _ => panic!(),
        }
    }

    /// Convert the value to `f64`
    ///
    /// # Safety
    ///
    /// Will return arbitrary values if `is_float` returns false
    #[inline]
    pub fn cast_float(self) -> f64 {
        match self {
            Value::Float(x) => x,
            _ => panic!(),
        }
    }

    /// Convert the value to [`Object`]
    ///
    /// # Safety
    ///
    /// Caller must guarentee that the value is an object
    #[inline]
    pub unsafe fn unsafe_cast_object(self) -> Gc<Object> {
        match self {
            Value::Object(x) => x,
            _ => panic!(),
        }
    }

    /// Convert the value to `String`
    ///
    /// # Safety
    ///
    /// Caller must guarentee that the value is an string
    #[inline]
    pub unsafe fn unsafe_cast_string(self) -> Gc<String> {
        match self {
            Value::String(x) => x,
            _ => panic!(),
        }
    }

    #[inline]
    pub fn into_int(self) -> Option<i32> {
        match self {
            Value::Integer(x) => Some(x),
            _ => None,
        }
    }

    #[inline]
    pub fn into_float(self) -> Option<f64> {
        match self {
            Value::Float(x) => Some(x),
            _ => None,
        }
    }

    #[inline]
    pub fn into_bool(self) -> Option<bool> {
        match self {
            Value::Boolean(x) => Some(x),
            _ => None,
        }
    }

    #[inline]
    pub fn into_string(self) -> Option<Gc<String>> {
        match self {
            Value::String(x) => Some(x),
            _ => None,
        }
    }

    #[inline]
    pub fn into_object(self) -> Option<Gc<Object>> {
        match self {
            Value::Object(x) => Some(x),
            _ => None,
        }
    }
}

impl From<bool> for Value {
    #[inline]
    fn from(v: bool) -> Value {
        Value::Boolean(v)
    }
}

impl From<i32> for Value {
    #[inline]
    fn from(v: i32) -> Value {
        Value::Integer(v)
    }
}

impl From<f64> for Value {
    #[inline]
    fn from(v: f64) -> Value {
        Value::Float(v)
    }
}

impl From<Gc<String>> for Value {
    #[inline]
    fn from(v: Gc<String>) -> Value {
        Value::String(v)
    }
}

impl From<Gc<Object>> for Value {
    #[inline]
    fn from(v: Gc<Object>) -> Value {
        Value::Object(v)
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
        match *self {
            Value::Object(x) => ctx.mark(x),
            Value::String(x) => ctx.mark(x),
            _ => {}
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Boolean(x) => f.debug_tuple("JSValue::Bool").field(&x).finish(),
            Value::Null => f.debug_tuple("JSValue::Null").finish(),
            Value::Undefined => f.debug_tuple("JSValue::Undefined").finish(),
            Value::Empty => f.debug_tuple("JSValue::Empty").finish(),
            Value::String(x) => f.debug_tuple("JSValue::String").field(&x).finish(),
            Value::Object(x) => f.debug_tuple("JSValue::Object").field(&x).finish(),
            Value::Integer(x) => f.debug_tuple("JSValue::Int").field(&x).finish(),
            Value::Float(x) => f.debug_tuple("JSValue::Float").field(&x).finish(),
        }
    }
}
