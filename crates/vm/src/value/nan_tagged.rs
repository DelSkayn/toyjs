use crate::{
    gc::{Ctx, Gc, Trace},
    Object,
};

//TODO rediscover implementation and document.

//mod tagged_union;
//pub use tagged_union::TaggedValue;

use std::{cmp, fmt};

pub const VALUE_EMPTY: u64 = 0x0;
pub const VALUE_DELETED: u64 = 0x5;
pub const VALUE_FALSE: u64 = 0x06;
pub const VALUE_TRUE: u64 = 0x07;
pub const VALUE_UNDEFINED: u64 = 0x0A;
pub const VALUE_NULL: u64 = 0x02;

pub const TAG_BASE: u64 = 0x0000_0000_0000_0000;
pub const TAG_OBJECT: u64 = 0x0001_0000_0000_0000;
pub const TAG_STRING: u64 = 0x0002_0000_0000_0000;
pub const TAG_BIGINT: u64 = 0x0003_0000_0000_0000;
pub const TAG_SYMBOL: u64 = 0x0004_0000_0000_0000;
pub const TAG_UNUSED1: u64 = 0x0005_0000_0000_0000;
//pub const TAG_UNUSED2: u64 = 0x0007_0000_0000_0000;
pub const TAG_INT: u64 = 0x0006_0000_0000_0000;

const MIN_FLOAT: u64 = 0x0007_0000_0000_0000;
const MIN_NUMBER: u64 = TAG_INT;
const TAG_MASK: u64 = 0xffff_0000_0000_0000;
const PTR_MASK: u64 = 0x0000_ffff_ffff_ffff;

#[derive(Clone, Copy)]
pub union ValueUnion {
    float: f64,
    int: i32,
    pub bits: u64,
}

impl cmp::Eq for ValueUnion {}
impl cmp::PartialEq<ValueUnion> for ValueUnion {
    fn eq(&self, other: &ValueUnion) -> bool {
        unsafe { self.bits == other.bits }
    }
}

/// The value representation used in the VM.
///
/// Toyjs uses nan-tagging for its value.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Value(pub ValueUnion);

impl Value {
    /// Returns wether two values have the same data type.
    #[inline]
    pub fn same_type(self, other: Value) -> bool {
        unsafe {
            let tag = self.0.bits & TAG_MASK;
            if tag != other.0.bits & TAG_MASK {
                return self.is_float() && other.is_float();
            }
            if tag == TAG_BASE {
                return self.0.bits == other.0.bits;
            }
            true
        }
    }

    /// Is this value a gc allocated value.
    #[inline]
    pub fn requires_gc(self) -> bool {
        self.is_object() || self.is_string()
    }

    /// Is this value a boolean.
    #[inline]
    pub fn is_empty(self) -> bool {
        unsafe { self.0.bits == VALUE_EMPTY }
    }

    /// Is this value a boolean.
    #[inline]
    pub fn is_bool(self) -> bool {
        unsafe { (self.0.bits & !1) == VALUE_FALSE }
    }

    /// Is this value the value false.
    #[inline]
    pub fn is_false(self) -> bool {
        unsafe { self.0.bits == VALUE_FALSE }
    }

    /// Is this value the value true.
    #[inline]
    pub fn is_true(self) -> bool {
        unsafe { self.0.bits == VALUE_TRUE }
    }

    /// Is this value the value null.
    #[inline]
    pub fn is_null(self) -> bool {
        unsafe { self.0.bits == VALUE_NULL }
    }

    /// Is this value the value null.
    #[inline]
    pub fn is_undefined(self) -> bool {
        unsafe { self.0.bits == VALUE_UNDEFINED }
    }

    /// Is this value null like.
    #[inline]
    pub fn is_nullish(self) -> bool {
        unsafe { (self.0.bits & !8) == VALUE_NULL }
    }

    /// Is this value a number type.
    #[inline]
    pub fn is_number(self) -> bool {
        unsafe { self.0.bits >= MIN_NUMBER }
    }

    /// Is this value a number integer.
    #[inline]
    pub fn is_int(self) -> bool {
        unsafe { self.0.bits & TAG_MASK == TAG_INT }
    }

    /// Is this value a number float.
    #[inline]
    pub fn is_float(self) -> bool {
        unsafe { self.0.bits >= MIN_FLOAT }
    }

    /// Is this value a object.
    #[inline]
    pub fn is_object(self) -> bool {
        unsafe { self.0.bits & TAG_MASK == TAG_OBJECT }
    }

    /// Is this value a string.
    #[inline]
    pub fn is_string(self) -> bool {
        unsafe { self.0.bits & TAG_MASK == TAG_STRING }
    }

    /// Is this value a function.
    #[inline]
    pub unsafe fn is_function(self) -> bool {
        self.is_object() && self.unsafe_cast_object().is_function()
    }

    /// Create a new value containing the undefined javascript value.
    #[inline]
    pub const fn undefined() -> Self {
        Value(ValueUnion {
            bits: VALUE_UNDEFINED,
        })
    }

    /// Create a new value containing the null javascript value.
    #[inline]
    pub const fn null() -> Self {
        Value(ValueUnion { bits: VALUE_NULL })
    }

    /// Create a new value which is empty.
    /// This value is vm internal only and should not be exposed outside of the vm.
    #[inline]
    pub const fn empty() -> Self {
        Value(ValueUnion { bits: VALUE_EMPTY })
    }

    #[inline]
    pub const fn nan() -> Self {
        unsafe {
            Value(ValueUnion {
                bits: std::mem::transmute::<f64, u64>(f64::NAN) + MIN_FLOAT,
            })
        }
    }

    /// Convert the value to `bool`
    ///
    /// # Safety
    ///
    /// Will return arbitrary values if `is_bool` returns false
    #[inline]
    pub fn cast_bool(self) -> bool {
        unsafe {
            debug_assert!(self.is_bool());
            self.0.bits == VALUE_TRUE
        }
    }

    /// Convert the value to `i32`
    ///
    /// # Safety
    ///
    /// Will return arbitrary values if `is_int` returns false
    #[inline]
    pub fn cast_int(self) -> i32 {
        unsafe {
            debug_assert!(self.is_int());
            self.0.int
        }
    }

    /// Convert the value to `f64`
    ///
    /// # Safety
    ///
    /// Will return arbitrary values if `is_float` returns false
    #[inline]
    pub fn cast_float(mut self) -> f64 {
        unsafe {
            debug_assert!(self.is_float());
            self.0.bits -= MIN_FLOAT;
            self.0.float
        }
    }

    /// Convert the value to [`Object`]
    ///
    /// # Safety
    ///
    /// Caller must guarentee that the value is an object
    #[inline]
    pub unsafe fn unsafe_cast_object(self) -> Gc<Object> {
        debug_assert!(self.is_object());
        Gc::from_raw((self.0.bits & PTR_MASK) as *mut ())
    }

    /// Convert the value to `String`
    ///
    /// # Safety
    ///
    /// Caller must guarentee that the value is an string
    #[inline]
    pub unsafe fn unsafe_cast_string(self) -> Gc<String> {
        debug_assert!(self.is_string());
        Gc::from_raw((self.0.bits & PTR_MASK) as *mut ())
    }
}

impl From<bool> for Value {
    #[inline]
    fn from(v: bool) -> Value {
        Value(ValueUnion {
            bits: if v { VALUE_TRUE } else { VALUE_FALSE },
        })
    }
}

impl From<i32> for Value {
    #[inline]
    fn from(v: i32) -> Value {
        Value(ValueUnion {
            bits: TAG_INT | v as u32 as u64,
        })
    }
}

impl From<f64> for Value {
    #[inline]
    fn from(v: f64) -> Value {
        Value(ValueUnion {
            bits: v.to_bits() + MIN_FLOAT,
        })
    }
}

impl From<Gc<String>> for Value {
    #[inline]
    fn from(v: Gc<String>) -> Value {
        Value(ValueUnion {
            bits: TAG_STRING | Gc::into_raw(v) as u64,
        })
    }
}

impl From<Gc<Object>> for Value {
    #[inline]
    fn from(v: Gc<Object>) -> Value {
        Value(ValueUnion {
            bits: TAG_OBJECT | Gc::into_raw(v) as u64,
        })
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
        if self.is_object() {
            unsafe { ctx.mark(self.unsafe_cast_object()) }
        } else if self.is_string() {
            unsafe { ctx.mark(self.unsafe_cast_string()) }
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            match self.0.bits & TAG_MASK {
                TAG_BASE => match self.0.bits {
                    VALUE_TRUE => f.debug_tuple("JSValue::Bool").field(&true).finish(),
                    VALUE_FALSE => f.debug_tuple("JSValue::Bool").field(&false).finish(),
                    VALUE_NULL => f.debug_tuple("JSValue::Null").finish(),
                    VALUE_UNDEFINED => f.debug_tuple("JSValue::Undefined").finish(),
                    VALUE_EMPTY => f.debug_tuple("JSValue::Empty").finish(),
                    VALUE_DELETED => f.debug_tuple("JSValue::Deleted").finish(),
                    _ => f.debug_tuple("JSvalue::INVALID_VALUE").finish(),
                },
                TAG_STRING => f
                    .debug_tuple("JSValue::String")
                    .field(&self.unsafe_cast_string())
                    .finish(),
                TAG_OBJECT => {
                    let mut obj = f.debug_tuple("JSValue::Object");
                    obj.finish()
                }
                TAG_BIGINT => todo!(),
                TAG_SYMBOL => todo!(),
                TAG_INT => f
                    .debug_tuple("JSValue::Int")
                    .field(&self.cast_int())
                    .finish(),
                _ => f
                    .debug_tuple("JSValue::Float")
                    .field(&self.cast_float())
                    .finish(),
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::Value;

    #[test]
    fn convert_i32() {
        fn test_value(v: i32) {
            assert_eq!(v, Value::from(v).cast_int());
        }

        test_value(-1);
        test_value(1);
        test_value(0);
        test_value(i32::MAX);
        test_value(i32::MIN);
    }

    #[test]
    fn convert_f64() {
        fn test_value(v: f64) {
            assert_eq!(v.to_bits(), Value::from(v).cast_float().to_bits());
        }

        test_value(-1.0);
        test_value(1.0);
        test_value(0.0);
        test_value(f64::MAX);
        test_value(f64::MIN);
        test_value(f64::INFINITY);
        test_value(f64::NEG_INFINITY);
        test_value(f64::NAN);
        test_value(f64::MIN_POSITIVE);
    }
}
