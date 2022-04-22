use std::{fmt, mem};

use crate::{
    gc::{Gc, Trace, Tracer},
    object::Object,
};

#[derive(Clone, Copy)]
pub enum Value<'gc, 'cell> {
    Float(f64),
    Integer(i32),
    Boolean(bool),
    String(Gc<'gc, 'cell, String>),
    Object(Gc<'gc, 'cell, Object<'gc, 'cell>>),
    //Atom(Atom),
    Undefined,
    Null,
    Empty,
}

impl<'gc, 'cell> Value<'gc, 'cell> {
    pub fn to_static(self) -> Option<Value<'static, 'static>> {
        match self {
            Value::Float(x) => Some(Value::Float(x)),
            Value::Integer(x) => Some(Value::Integer(x)),
            Value::Boolean(x) => Some(Value::Boolean(x)),
            Value::Undefined => Some(Value::Undefined),
            Value::Null => Some(Value::Null),
            Value::Empty => Some(Value::Empty),
            _ => None,
        }
    }

    /// Returns wether two values have the same data type.
    #[inline]
    pub fn same_type(self, other: Self) -> bool {
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

    /*
    /// Is this value a string.
    #[inline]
    pub fn is_atom(self) -> bool {
        match self {
            Value::Atom(_) => true,
            _ => false,
        }
    }
    */

    /*
    /// Is this value a function.
    #[inline]
    pub unsafe fn is_function(self) -> bool {
        self.is_object() && self.unsafe_cast_object().is_function()
    }
    */

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
    pub fn into_string(self) -> Option<Gc<'gc, 'cell, String>> {
        match self {
            Value::String(x) => Some(x),
            _ => None,
        }
    }

    #[inline]
    pub fn into_object(self) -> Option<Gc<'gc, 'cell, Object<'gc, 'cell>>> {
        match self {
            Value::Object(x) => Some(x),
            _ => None,
        }
    }

    /*
    #[inline]
    pub fn into_atom(self) -> Option<Atom> {
        match self {
            Value::Atom(x) => Some(x),
            _ => None,
        }
    }
    */
}

impl<'gc, 'cell> From<bool> for Value<'gc, 'cell> {
    #[inline]
    fn from(v: bool) -> Self {
        Value::Boolean(v)
    }
}

impl<'gc, 'cell> From<i32> for Value<'gc, 'cell> {
    #[inline]
    fn from(v: i32) -> Self {
        Value::Integer(v)
    }
}

impl<'gc, 'cell> From<f64> for Value<'gc, 'cell> {
    #[inline]
    fn from(v: f64) -> Self {
        Value::Float(v)
    }
}

impl<'gc, 'cell> From<Gc<'gc, 'cell, String>> for Value<'gc, 'cell> {
    #[inline]
    fn from(v: Gc<'gc, 'cell, String>) -> Self {
        Value::String(v)
    }
}

impl<'gc, 'cell> From<Gc<'gc, 'cell, Object<'gc, 'cell>>> for Value<'gc, 'cell> {
    #[inline]
    fn from(v: Gc<'gc, 'cell, Object<'gc, 'cell>>) -> Self {
        Value::Object(v)
    }
}

/*
impl From<Atom> for Value {
    #[inline]
    fn from(v: Atom) -> Value {
        Value::Atom(v)
    }
}
*/

unsafe impl<'gc, 'cell> Trace<'cell> for Value<'gc, 'cell> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, ctx: Tracer<'a, 'cell>) {
        match *self {
            Value::Object(x) => ctx.mark(x),
            Value::String(x) => ctx.mark(x),
            _ => {}
        }
    }
}

impl<'gc, 'cell> fmt::Debug for Value<'gc, 'cell> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Boolean(x) => f.debug_tuple("JSValue::Bool").field(&x).finish(),
            Value::Null => f.debug_tuple("JSValue::Null").finish(),
            Value::Undefined => f.debug_tuple("JSValue::Undefined").finish(),
            Value::Empty => f.debug_tuple("JSValue::Empty").finish(),
            Value::String(_) => f.debug_tuple("JSValue::String").finish(),
            Value::Object(_) => f.debug_tuple("JSValue::Object").finish(),
            //Value::Atom(x) => f.debug_tuple("JSValue::Atom").field(&x).finish(),
            Value::Integer(x) => f.debug_tuple("JSValue::Int").field(&x).finish(),
            Value::Float(x) => f.debug_tuple("JSValue::Float").field(&x).finish(),
        }
    }
}
