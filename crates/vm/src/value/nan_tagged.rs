use crate::{
    function::Function,
    gc::{Ctx, Gc, Trace},
    Object,
};

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
pub const TAG_SYMBOL: u64 = 0x0004_0000_0000_0000; //?
pub const TAG_FUNCTION: u64 = 0x0005_0000_0000_0000; //?
pub const TAG_INT: u64 = 0x0006_0000_0000_0000;

const MIN_FLOAT: u64 = 0x0007_0000_0000_0000;
const MIN_NUMBER: u64 = (TAG_INT as u64) << 48;
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

    #[inline]
    pub fn is_bool(self) -> bool {
        unsafe { (self.0.bits & !1) == VALUE_FALSE }
    }

    #[inline]
    pub fn is_false(self) -> bool {
        unsafe { self.0.bits == VALUE_FALSE }
    }

    #[inline]
    pub fn is_true(self) -> bool {
        unsafe { self.0.bits == VALUE_TRUE }
    }

    #[inline]
    pub fn is_null(self) -> bool {
        unsafe { self.0.bits == VALUE_NULL }
    }

    #[inline]
    pub fn is_undefined(self) -> bool {
        unsafe { self.0.bits == VALUE_UNDEFINED }
    }

    #[inline]
    pub fn is_nullish(self) -> bool {
        unsafe { (self.0.bits & !8) == VALUE_NULL }
    }

    #[inline]
    pub fn is_number(self) -> bool {
        unsafe { self.0.bits >= MIN_NUMBER }
    }

    #[inline]
    pub fn is_int(self) -> bool {
        unsafe { self.0.bits & TAG_MASK == TAG_INT }
    }

    #[inline]
    pub fn is_float(self) -> bool {
        unsafe { self.0.bits >= MIN_FLOAT }
    }

    #[inline]
    pub fn is_object(self) -> bool {
        unsafe { self.0.bits & TAG_MASK == TAG_OBJECT }
    }

    #[inline]
    pub fn is_string(self) -> bool {
        unsafe { self.0.bits & TAG_MASK == TAG_STRING }
    }

    #[inline]
    pub fn is_function(self) -> bool {
        unsafe { self.0.bits & TAG_MASK == TAG_FUNCTION }
    }

    #[inline]
    pub fn undefined() -> Self {
        Value(ValueUnion {
            bits: VALUE_UNDEFINED,
        })
    }

    #[inline]
    pub fn null() -> Self {
        Value(ValueUnion { bits: VALUE_NULL })
    }

    #[inline]
    pub fn empty() -> Self {
        Value(ValueUnion { bits: VALUE_EMPTY })
    }

    #[inline]
    pub unsafe fn from_ptr(ptr: *mut ()) -> Self {
        Value(ValueUnion {
            bits: ptr as u64 & PTR_MASK,
        })
    }

    #[inline]
    pub unsafe fn from_const_ptr(ptr: *const ()) -> Self {
        Value(ValueUnion {
            bits: ptr as u64 & PTR_MASK,
        })
    }

    #[inline]
    pub fn cast_bool(self) -> bool {
        unsafe {
            debug_assert!(self.is_bool());
            self.0.bits == VALUE_TRUE
        }
    }

    #[inline]
    pub fn cast_int(self) -> i32 {
        unsafe {
            debug_assert!(self.is_int());
            self.0.int
        }
    }

    #[inline]
    pub fn cast_float(mut self) -> f64 {
        unsafe {
            debug_assert!(self.is_float());
            self.0.bits -= MIN_FLOAT;
            self.0.float
        }
    }

    #[inline]
    pub unsafe fn unsafe_cast_object(self) -> Gc<Object> {
        debug_assert!(self.is_object());
        Gc::from_raw((self.0.bits & PTR_MASK) as *mut ())
    }

    #[inline]
    pub unsafe fn unsafe_cast_function(self) -> Gc<Function> {
        debug_assert!(self.is_function());
        Gc::from_raw((self.0.bits & PTR_MASK) as *mut ())
    }

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
        unsafe {
            let mut val = ValueUnion { float: v };
            val.bits += MIN_FLOAT;
            Value(val)
        }
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

impl From<Gc<Function>> for Value {
    #[inline]
    fn from(v: Gc<Function>) -> Value {
        Value(ValueUnion {
            bits: TAG_FUNCTION | Gc::into_raw(v) as u64,
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
                TAG_FUNCTION => {
                    let mut obj = f.debug_tuple("JSValue::Function");
                    obj.finish()
                }
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
