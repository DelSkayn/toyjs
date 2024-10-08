//use common::atom::Atom;

//TODO rediscover implementation and document.

//mod tagged_union;
//pub use tagged_union::TaggedValue;
use std::{
    cmp, fmt,
    marker::PhantomData,
    num::{NonZeroU64, NonZeroUsize},
    ptr::NonNull,
};

use bc::Primitive;
use common::{
    map_ptr,
    ptr::{Raw, RawCell},
};

use crate::{
    gc::{self, Free, Gc, RootState, Rooted, Trace},
    object::GcObject,
};

pub const VALUE_EMPTY: u64 = 0x0;
pub const VALUE_NULL: u64 = 0x02;
pub const VALUE_DELETED: u64 = 0x5;
pub const VALUE_FALSE: u64 = 0x06;
pub const VALUE_TRUE: u64 = 0x07;
pub const VALUE_UNDEFINED: u64 = 0x0A;

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
    ptr: Raw<u8>,
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
pub struct Value<R: RootState> {
    value: RawCell<ValueUnion>,
    marker: PhantomData<R>,
}

impl<R: RootState> Clone for Value<R> {
    fn clone(&self) -> Self {
        let v = unsafe { self.value.get().read() };
        Self {
            value: RawCell::new(v),
            marker: PhantomData,
        }
    }
}

impl<R: RootState> Value<R> {
    #[inline]
    pub fn bits(&self) -> u64 {
        unsafe { self.value.get().map_ptr(map_ptr!(ValueUnion, bits)).read() }
    }

    #[inline]
    pub fn tag(&self) -> u64 {
        self.bits() & TAG_MASK
    }

    unsafe fn from_value(v: ValueUnion) -> Self {
        Value {
            value: RawCell::new(v),
            marker: PhantomData,
        }
    }

    /// Turns a value into a rooted value if the internal value is not gc allocated.
    #[inline]
    pub fn to_rooted(self) -> Option<Value<Rooted>> {
        let tag = self.tag();
        if tag == TAG_OBJECT || tag == TAG_STRING {
            return None;
        }

        Some(Value {
            value: self.value,
            marker: PhantomData,
        })
    }

    /// Returns wether two values have the same data type.
    #[inline]
    pub fn same_type<O: RootState>(self, other: &Value<O>) -> bool {
        let tag = self.tag();
        if tag != other.tag() {
            return self.is_float() && other.is_float();
        }
        if tag == TAG_BASE {
            return self.bits() == other.bits();
        }
        true
    }

    /// Is this value a gc allocated value.
    #[inline]
    pub fn requires_gc(&self) -> bool {
        self.is_object() || self.is_string()
    }

    /// Is this value a boolean.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.bits() == VALUE_EMPTY
    }

    /// Is this value a boolean.
    #[inline]
    pub fn is_bool(&self) -> bool {
        (self.bits() & !1) == VALUE_FALSE
    }

    /// Is this value the value false.
    #[inline]
    pub fn is_false(&self) -> bool {
        self.bits() == VALUE_FALSE
    }

    /// Is this value the value true.
    #[inline]
    pub fn is_true(&self) -> bool {
        self.bits() == VALUE_TRUE
    }

    /// Is this value the value null.
    #[inline]
    pub fn is_null(&self) -> bool {
        self.bits() == VALUE_NULL
    }

    /// Is this value the value null.
    #[inline]
    pub fn is_undefined(&self) -> bool {
        self.bits() == VALUE_UNDEFINED
    }

    /// Is this value null like.
    #[inline]
    pub fn is_nullish(&self) -> bool {
        (self.bits() & !8) == VALUE_NULL
    }

    /// Is this value a number type.
    #[inline]
    pub fn is_number(&self) -> bool {
        self.bits() >= MIN_NUMBER
    }

    /// Is this value a number integer.
    #[inline]
    pub fn is_int(&self) -> bool {
        self.tag() == TAG_INT
    }

    /// Is this value a number float.
    #[inline]
    pub fn is_float(&self) -> bool {
        self.bits() >= MIN_FLOAT
    }

    /// Is this value a object.
    #[inline]
    pub fn is_object(&self) -> bool {
        self.tag() == TAG_OBJECT
    }

    /// Is this value a string.
    #[inline]
    pub fn is_string(&self) -> bool {
        self.tag() == TAG_STRING
    }

    #[inline]
    pub fn is_atom(&self) -> bool {
        self.tag() == TAG_ATOM
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
    pub fn ensure_float(v: f64) -> Value<Rooted> {
        Value {
            value: RawCell::new(ValueUnion {
                bits: v.to_bits() + MIN_FLOAT,
            }),
            marker: PhantomData,
        }
    }

    #[inline]
    pub fn into_int(&self) -> Option<i32> {
        if self.is_int() {
            unsafe { Some(self.value.get().map_ptr(map_ptr!(ValueUnion, int)).read()) }
        } else {
            None
        }
    }

    #[inline]
    pub fn into_float(&self) -> Option<f64> {
        if self.is_float() {
            let v = self.bits() - MIN_FLOAT;
            Some(f64::from_bits(v))
        } else {
            None
        }
    }

    #[inline]
    pub fn into_bool(&self) -> Option<bool> {
        if self.is_bool() {
            Some(self.bits() == VALUE_TRUE)
        } else {
            None
        }
    }

    /*
    #[inline]
    pub fn into_string(self) -> Option<Gc<'gc, 'own, String>> {
        unsafe {
            if self.is_string() {
                let ptr = (self.value.bits & PTR_MASK) as *mut _;
                let ptr = NonNull::new_unchecked(ptr);
                Some(Gc::from_gc_box(ptr))
            } else {
                None
            }
        }
    }
    */

    #[inline]
    pub fn as_object(&self) -> Option<GcObject<R>> {
        if self.is_object() {
            unsafe {
                let addr = NonZeroUsize::new_unchecked((self.bits() & PTR_MASK) as usize);
                let ptr = Raw::from_exposed_addr(addr);
                Some(Gc::from_raw(ptr))
            }
        } else {
            None
        }
    }

    /*
    #[inline]
    pub fn into_atom(self) -> Option<Atom<'gc, 'own>> {
        if self.is_atom() {
            unsafe { Some(Atom::from_raw(self.value.ptr.cast())) }
        } else {
            None
        }
    }
    */
}

impl From<bool> for Value<Rooted> {
    #[inline]
    fn from(v: bool) -> Self {
        unsafe {
            Value::from_value(ValueUnion {
                bits: if v { VALUE_TRUE } else { VALUE_FALSE },
            })
        }
    }
}

impl From<Primitive> for Value<Rooted> {
    #[inline]
    fn from(v: Primitive) -> Self {
        unsafe { Value::from_value(ValueUnion { bits: v.0 as u64 }) }
    }
}

impl From<i32> for Value<Rooted> {
    #[inline]
    fn from(v: i32) -> Self {
        unsafe {
            Value::from_value(ValueUnion {
                bits: TAG_INT | v as u32 as u64,
            })
        }
    }
}

impl From<f64> for Value<Rooted> {
    #[inline]
    fn from(v: f64) -> Self {
        if v as i32 as f64 == v {
            (v as i32).into()
        } else {
            Self::ensure_float(v)
        }
    }
}

impl<R: RootState> From<Gc<R, String>> for Value<R> {
    #[inline]
    fn from(v: Gc<R, String>) -> Self {
        unsafe {
            Value::from_value(ValueUnion {
                bits: TAG_STRING | Gc::as_raw(v).expose_addr().get() as u64,
            })
        }
    }
}

impl<R: RootState> From<GcObject<R>> for Value<R> {
    #[inline]
    fn from(v: GcObject<R>) -> Self {
        unsafe {
            Value::from_value(ValueUnion {
                bits: TAG_OBJECT | Gc::as_raw(v).expose_addr().get() as u64,
            })
        }
    }
}

/*
impl<'gc, 'own> From<Atom<'gc, 'own>> for Value<'gc, 'own> {
    #[inline]
    fn from(v: Atom<'gc, 'own>) -> Self {
        unsafe {
            Value::from_value(ValueUnion {
                bits: TAG_ATOM | v.into_raw() as u32 as u64,
            })
        }
    }
}
*/

unsafe impl<R: RootState> Trace for Value<R> {
    type Free<'a> = Value<Free<'a>>;
    type Rooted = Value<Rooted>;

    const NEEDS_TRACE: bool = true;

    fn trace(&self, marker: &gc::Marker) -> Result<(), gc::Error> {
        marker.mark_value(self)
    }
}

impl<R: RootState> fmt::Debug for Value<R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.bits() & TAG_MASK {
            TAG_BASE => match self.bits() {
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
            /*
            TAG_ATOM => f
                .debug_tuple("JSValue::ATOM")
                .field(&self.into_atom().unwrap())
                .finish(),
                */
            TAG_INT => f
                .debug_tuple("JSValue::Int")
                .field(&self.clone().into_int().unwrap())
                .finish(),
            _ => f
                .debug_tuple("JSValue::Float")
                .field(&self.clone().into_float().unwrap())
                .finish(),
        }
    }
}
