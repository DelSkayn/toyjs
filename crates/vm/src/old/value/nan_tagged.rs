use common::atom::Atom;


use dreck::{Bound, Gc, Trace, Tracer, marker::Invariant};

use crate::{
    GcObject, Object,
};

//TODO rediscover implementation and document.

//mod tagged_union;
//pub use tagged_union::TaggedValue;

use std::{cmp, fmt, marker::PhantomData};

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
pub const TAG_ATOM: u64 = 0x0004_0000_0000_0000;
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
pub struct Value<'gc, 'own> {
    value: ValueUnion,
    marker: PhantomData<&'gc ()>,
    id: Invariant<'own>,
}

impl<'gc, 'own> Value<'gc, 'own> {
    unsafe fn from_value(v: ValueUnion) -> Self {
        Value {
            value: v,
            marker: PhantomData,
            id: Invariant::new(),
        }
    }

    #[inline]
    pub fn empty_to_undefined(self) -> Self {
        unsafe {
            if self.value.bits == VALUE_EMPTY {
                Value::undefined()
            } else {
                self
            }
        }
    }

    #[inline]
    pub fn to_static(self) -> Option<Value<'static, 'own>> {
        unsafe {
            let tag = self.value.bits & TAG_MASK;
            if tag == TAG_OBJECT || tag == TAG_STRING {
                return None;
            }

            Some(Value {
                value: self.value,
                marker: PhantomData,
                id: Invariant::new(),
            })
        }
    }

    /// Returns wether two values have the same data type.
    #[inline]
    pub fn same_type(self, other: Value) -> bool {
        unsafe {
            let tag = self.value.bits & TAG_MASK;
            if tag != other.value.bits & TAG_MASK {
                return self.is_float() && other.is_float();
            }
            if tag == TAG_BASE {
                return self.value.bits == other.value.bits;
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
        unsafe { self.value.bits == VALUE_EMPTY }
    }

    /// Is this value a boolean.
    #[inline]
    pub fn is_bool(self) -> bool {
        unsafe { (self.value.bits & !1) == VALUE_FALSE }
    }

    /// Is this value the value false.
    #[inline]
    pub fn is_false(self) -> bool {
        unsafe { self.value.bits == VALUE_FALSE }
    }

    /// Is this value the value true.
    #[inline]
    pub fn is_true(self) -> bool {
        unsafe { self.value.bits == VALUE_TRUE }
    }

    /// Is this value the value null.
    #[inline]
    pub fn is_null(self) -> bool {
        unsafe { self.value.bits == VALUE_NULL }
    }

    /// Is this value the value null.
    #[inline]
    pub fn is_undefined(self) -> bool {
        unsafe { self.value.bits == VALUE_UNDEFINED }
    }

    /// Is this value null like.
    #[inline]
    pub fn is_nullish(self) -> bool {
        unsafe { (self.value.bits & !8) == VALUE_NULL }
    }

    /// Is this value a number type.
    #[inline]
    pub fn is_number(self) -> bool {
        unsafe {
            self.value.bits >= MIN_NUMBER
        }
    }

    /// Is this value a number integer.
    #[inline]
    pub fn is_int(self) -> bool {
        unsafe { self.value.bits & TAG_MASK == TAG_INT }
    }

    /// Is this value a number float.
    #[inline]
    pub fn is_float(self) -> bool {
        unsafe { self.value.bits >= MIN_FLOAT }
    }

    /// Is this value a object.
    #[inline]
    pub fn is_object(self) -> bool {
        unsafe { self.value.bits & TAG_MASK == TAG_OBJECT }
    }

    /// Is this value a string.
    #[inline]
    pub fn is_string(self) -> bool {
        unsafe { self.value.bits & TAG_MASK == TAG_STRING }
    }

    #[inline]
    pub fn is_atom(self) -> bool {
        unsafe { self.value.bits & TAG_MASK == TAG_ATOM }
    }

    /// Create a new value containing the undefined javascript value.
    #[inline]
    pub fn undefined() -> Self {
        unsafe {
            Value::from_value(ValueUnion {
                bits: VALUE_UNDEFINED,
            })
        }
    }

    /// Create a new value containing the null javascript value.
    #[inline]
    pub fn null() -> Self {
        unsafe { Value::from_value(ValueUnion { bits: VALUE_NULL }) }
    }

    /// Create a new value which is empty.
    /// This value is vm internal only and should not be exposed outside of the vm.
    #[inline]
    pub fn empty() -> Self {
        unsafe { Value::from_value(ValueUnion { bits: VALUE_EMPTY }) }
    }

    #[inline]
    pub fn nan() -> Self {
        unsafe {
            Value::from_value(ValueUnion {
                bits: f64::NAN.to_bits() + MIN_FLOAT,
            })
        }
    }

    #[inline]
    pub fn ensure_float(v: f64) -> Self {
        unsafe {
            Value {
                value: ValueUnion {
                    bits: v.to_bits() + MIN_FLOAT,
                },
                marker: PhantomData,
                id: Invariant::new(),
            }
        }
    }

    #[inline]
    pub fn into_int(self) -> Option<i32> {
        if self.is_int() {
            unsafe { Some(self.value.int) }
        } else {
            None
        }
    }

    #[inline]
    pub fn into_float(mut self) -> Option<f64> {
        if self.is_float() {
            unsafe {
                self.value.bits -= MIN_FLOAT;
                Some(self.value.float)
            }
        } else {
            None
        }
    }

    #[inline]
    pub fn into_bool(self) -> Option<bool> {
        if self.is_bool() {
            unsafe { Some(self.value.bits == VALUE_TRUE) }
        } else {
            None
        }
    }

    #[inline]
    pub fn into_string(self) -> Option<Gc<'gc, 'own, String>> {
        unsafe {
            if self.is_string() {
                let ptr = (self.value.bits & PTR_MASK) as *mut _;
                Some(Gc::from_raw(ptr))
            } else {
                None
            }
        }
    }

    #[inline]
    pub fn into_object(self) -> Option<GcObject<'gc, 'own>> {
        unsafe {
            if self.is_object() {
                let ptr = (self.value.bits & PTR_MASK) as *const Object<'gc, 'own>;
                Some(Gc::from_raw(ptr))
            } else {
                None
            }
        }
    }

    #[inline]
    pub fn into_atom(self) -> Option<Atom> {
        if self.is_atom() {
            unsafe { Some(Atom::from_raw(self.value.int as u32)) }
        } else {
            None
        }
    }
}

impl<'gc, 'own> From<bool> for Value<'gc, 'own> {
    #[inline]
    fn from(v: bool) -> Self {
        unsafe {
            Value::from_value(ValueUnion {
                bits: if v { VALUE_TRUE } else { VALUE_FALSE },
            })
        }
    }
}

impl<'gc, 'own> From<i32> for Value<'gc, 'own> {
    #[inline]
    fn from(v: i32) -> Self {
        unsafe {
            Value::from_value(ValueUnion {
                bits: TAG_INT | v as u32 as u64,
            })
        }
    }
}

impl<'gc, 'own> From<f64> for Value<'gc, 'own> {
    #[inline]
    fn from(v: f64) -> Self {
        if v as i32 as f64 == v {
            (v as i32).into()
        } else {
            Self::ensure_float(v)
        }
    }
}

impl<'gc, 'own> From<Gc<'gc, 'own, String>> for Value<'gc, 'own> {
    #[inline]
    fn from(v: Gc<String>) -> Self {
        unsafe {
            Value::from_value(ValueUnion {
                bits: TAG_STRING | Gc::into_raw(v) as u64,
            })
        }
    }
}

impl<'gc, 'own> From<GcObject<'gc, 'own>> for Value<'gc, 'own> {
    #[inline]
    fn from(v: GcObject<'gc,'own>) -> Self {
        unsafe {
            Value::from_value(ValueUnion {
                bits: TAG_OBJECT | Gc::into_raw(v) as u64,
            })
        }
    }
}

impl<'gc, 'own> From<Atom> for Value<'gc, 'own> {
    #[inline]
    fn from(v: Atom) -> Self {
        unsafe {
            Value::from_value(ValueUnion {
                bits: TAG_ATOM | v.into_raw() as u32 as u64,
            })
        }
    }
}

unsafe impl<'gc, 'own> Trace<'own> for Value<'gc, 'own> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, tracer: Tracer<'a,'own>) {
        if let Some(obj) = self.into_object() {
            tracer.mark(obj);
        } else if let Some(s) = self.into_string() {
            tracer.mark(s);
        }
    }
}

unsafe impl<'a, 'gc, 'own> Bound<'a> for Value<'gc, 'own> {
    type Rebound = Value<'a, 'own>;
}

impl<'gc, 'own> fmt::Debug for Value<'gc, 'own> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            match self.value.bits & TAG_MASK {
                TAG_BASE => match self.value.bits {
                    VALUE_TRUE => f.debug_tuple("JSValue::Bool").field(&true).finish(),
                    VALUE_FALSE => f.debug_tuple("JSValue::Bool").field(&false).finish(),
                    VALUE_NULL => f.debug_tuple("JSValue::Null").finish(),
                    VALUE_UNDEFINED => f.debug_tuple("JSValue::Undefined").finish(),
                    VALUE_EMPTY => f.debug_tuple("JSValue::Empty").finish(),
                    VALUE_DELETED => f.debug_tuple("JSValue::Deleted").finish(),
                    _ => f.debug_tuple("JSvalue::INVALID_VALUE").finish(),
                },
                TAG_STRING => f.debug_tuple("JSValue::String").finish(),
                TAG_OBJECT => f.debug_tuple("JSValue::Object").finish(),
                TAG_BIGINT => todo!("big int"),
                TAG_ATOM => f
                    .debug_tuple("JSValue::ATOM")
                    .field(&self.into_atom().unwrap())
                    .finish(),
                TAG_INT => f
                    .debug_tuple("JSValue::Int")
                    .field(&self.into_int().unwrap())
                    .finish(),
                _ => f
                    .debug_tuple("JSValue::Float")
                    .field(&self.into_float().unwrap())
                    .finish(),
            }
        }
    }
}
