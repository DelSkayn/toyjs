use super::Value;
use crate::{function::Function, gc::BoundGc, object::Object};
use std::{cell::Cell, convert::TryFrom, error::Error, fmt, marker::PhantomData};

/// An error returend when trying to convert a value into the wrong type.
#[derive(Debug)]
pub struct ValueConversionError;

impl Error for ValueConversionError {}

impl fmt::Display for ValueConversionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Tried to convert value to a type which it did not contain"
        )
    }
}

/// A value bound to a realm
///
/// Values can contain garbage collected pointers.
/// Holding on to a pointer between garbage collection could result in the held value being freed.
///
/// Bound values hold an immutable reference to the realm they where returned from, as executing
/// code requires a mutable refrence it is impossible to hold onto a bound value during garbage
/// collection.
#[derive(Clone, Copy, Debug)]
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

    pub fn undefined() -> BoundValue<'a> {
        unsafe { Self::bind(Value::undefined()) }
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

impl<'a> TryFrom<BoundValue<'a>> for BoundGc<'a, String> {
    type Error = ValueConversionError;
    fn try_from(v: BoundValue<'a>) -> Result<BoundGc<'a, String>, Self::Error> {
        if v.is_string() {
            Ok(unsafe { v.unsafe_cast_string() })
        } else {
            Err(ValueConversionError)
        }
    }
}

impl<'a> TryFrom<BoundValue<'a>> for i32 {
    type Error = ValueConversionError;
    fn try_from(v: BoundValue<'a>) -> Result<Self, Self::Error> {
        if v.is_int() {
            Ok(v.cast_int())
        } else {
            Err(ValueConversionError)
        }
    }
}

impl<'a> TryFrom<BoundValue<'a>> for f64 {
    type Error = ValueConversionError;
    fn try_from(v: BoundValue<'a>) -> Result<Self, Self::Error> {
        if v.is_int() {
            Ok(v.cast_int() as f64)
        } else if v.is_float() {
            Ok(v.cast_float())
        } else {
            Err(ValueConversionError)
        }
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
