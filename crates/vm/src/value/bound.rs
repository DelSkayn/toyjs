use super::Value;
use crate::{function::Function, gc::BoundGc, object::Object};
use std::{cell::Cell, marker::PhantomData};

/// A value bound to a realm
///
/// Values can contain garbage collected pointers.
/// Holding on to a pointer between garbage collection could result in the held value being freed.
///
/// Bound values hold an immutable reference to the realm they where returned from, as executing
/// code requires a mutable refrence it is impossible to hold onto a bound value during garbage
/// collection.
pub struct BoundValue<'a> {
    value: Value,
    marker: PhantomData<Cell<&'a ()>>,
}

impl<'a> BoundValue<'a> {
    pub unsafe fn bind(value: Value) -> BoundValue<'a> {
        BoundValue {
            value,
            marker: PhantomData,
        }
    }

    pub fn unbind(self) -> Value {
        self.value
    }

    pub fn into_inner(self) -> Value {
        self.value
    }

    #[inline]
    pub fn is_bool(self) -> bool {
        self.value.is_bool()
    }

    #[inline]
    pub fn is_false(self) -> bool {
        self.value.is_false()
    }

    #[inline]
    pub fn is_true(self) -> bool {
        self.value.is_true()
    }

    #[inline]
    pub fn is_null(self) -> bool {
        self.value.is_null()
    }

    #[inline]
    pub fn is_undefined(self) -> bool {
        self.value.is_undefined()
    }

    #[inline]
    pub fn is_nullish(self) -> bool {
        self.value.is_nullish()
    }

    #[inline]
    pub fn is_number(self) -> bool {
        self.value.is_number()
    }

    #[inline]
    pub fn is_int(self) -> bool {
        self.value.is_int()
    }

    #[inline]
    pub fn is_float(self) -> bool {
        self.value.is_float()
    }

    #[inline]
    pub fn is_object(self) -> bool {
        self.value.is_object()
    }

    #[inline]
    pub fn is_string(self) -> bool {
        self.value.is_string()
    }

    #[inline]
    pub fn is_function(self) -> bool {
        self.value.is_function()
    }

    #[inline]
    pub fn cast_bool(self) -> bool {
        self.value.cast_bool()
    }

    #[inline]
    pub fn cast_int(self) -> i32 {
        self.value.cast_int()
    }

    #[inline]
    pub fn cast_float(self) -> f64 {
        self.value.cast_float()
    }

    #[inline]
    pub unsafe fn unsafe_cast_object(self) -> BoundGc<'a, Object> {
        BoundGc::bind(self.value.unsafe_cast_object())
    }

    #[inline]
    pub unsafe fn unsafe_cast_function(self) -> BoundGc<'a, Function> {
        BoundGc::bind(self.value.unsafe_cast_function())
    }

    #[inline]
    pub unsafe fn unsafe_cast_string(self) -> BoundGc<'a, String> {
        BoundGc::bind(self.value.unsafe_cast_string())
    }
}

impl<'a> From<BoundGc<'a, Object>> for BoundValue<'a> {
    fn from(v: BoundGc<'a, Object>) -> Self {
        unsafe { Self::bind(Value::from(v.unbind())) }
    }
}

impl<'a> From<BoundGc<'a, Function>> for BoundValue<'a> {
    fn from(v: BoundGc<'a, Function>) -> Self {
        unsafe { Self::bind(Value::from(v.unbind())) }
    }
}

impl<'a> From<BoundGc<'a, String>> for BoundValue<'a> {
    fn from(v: BoundGc<'a, String>) -> Self {
        unsafe { Self::bind(Value::from(v.unbind())) }
    }
}
