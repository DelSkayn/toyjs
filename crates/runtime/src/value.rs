use crate::{
    gc::{Ctx, Gc, Trace},
    object::Object,
};
use std::{cell::RefCell, cmp, fmt, hash, marker::PhantomData, mem, rc::Rc};

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
pub union JSValueUnion {
    float: f64,
    int: i32,
    pub bits: u64,
}

impl cmp::Eq for JSValueUnion {}
impl cmp::PartialEq<JSValueUnion> for JSValueUnion {
    fn eq(&self, other: &JSValueUnion) -> bool {
        unsafe { self.bits == other.bits }
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct JSValue(pub JSValueUnion);

impl JSValue {
    #[inline]
    pub fn tag(self) -> u64 {
        unsafe { self.0.bits & TAG_MASK }
    }

    #[inline]
    pub fn is_bool(self) -> bool {
        unsafe { (self.0.bits & !1) == VALUE_FALSE }
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
        JSValue(JSValueUnion {
            bits: VALUE_UNDEFINED,
        })
    }

    #[inline]
    pub fn null() -> Self {
        JSValue(JSValueUnion { bits: VALUE_NULL })
    }

    #[inline]
    pub fn empty() -> Self {
        JSValue(JSValueUnion { bits: VALUE_EMPTY })
    }

    #[inline]
    pub unsafe fn from_ptr(ptr: *mut ()) -> Self {
        JSValue(JSValueUnion {
            bits: ptr as u64 & PTR_MASK,
        })
    }

    #[inline]
    pub unsafe fn from_const_ptr(ptr: *const ()) -> Self {
        JSValue(JSValueUnion {
            bits: ptr as u64 & PTR_MASK,
        })
    }

    #[inline]
    pub fn into_bool(self) -> bool {
        unsafe {
            debug_assert!(self.is_bool());
            self.0.bits == VALUE_TRUE
        }
    }

    #[inline]
    pub fn into_int(self) -> i32 {
        unsafe {
            debug_assert!(self.is_int());
            self.0.int
        }
    }

    #[inline]
    pub fn into_float(mut self) -> f64 {
        unsafe {
            debug_assert!(self.is_float());
            dbg!(self.0.bits);
            self.0.bits -= MIN_FLOAT;
            self.0.float
        }
    }

    #[inline]
    pub unsafe fn into_object(self) -> Gc<Object> {
        debug_assert!(self.is_object());
        Gc::from_raw((self.0.bits & PTR_MASK) as *mut ())
    }

    #[inline]
    pub unsafe fn into_string(self) -> Gc<String> {
        debug_assert!(self.is_string());
        Gc::from_raw((self.0.bits & PTR_MASK) as *mut ())
    }

    #[inline]
    pub unsafe fn into_ptr(self) -> *mut () {
        debug_assert!(self.0.bits & TAG_MASK == 0);
        self.0.bits as *mut ()
    }

    #[inline]
    pub unsafe fn into_const_ptr(self) -> *const () {
        debug_assert!(self.0.bits & TAG_MASK == 0);
        self.0.bits as *const ()
    }
}

impl From<bool> for JSValue {
    #[inline]
    fn from(v: bool) -> JSValue {
        JSValue(JSValueUnion {
            bits: if v { VALUE_TRUE } else { VALUE_FALSE },
        })
    }
}

impl From<i32> for JSValue {
    #[inline]
    fn from(v: i32) -> JSValue {
        JSValue(JSValueUnion {
            bits: TAG_INT | v as u32 as u64,
        })
    }
}

impl From<f64> for JSValue {
    #[inline]
    fn from(v: f64) -> JSValue {
        unsafe {
            let mut val = JSValueUnion { float: v };
            val.bits += MIN_FLOAT;
            JSValue(val)
        }
    }
}

impl From<Gc<String>> for JSValue {
    #[inline]
    fn from(v: Gc<String>) -> JSValue {
        JSValue(JSValueUnion {
            bits: TAG_STRING | Gc::into_raw(v) as u64,
        })
    }
}

impl From<Gc<Object>> for JSValue {
    #[inline]
    fn from(v: Gc<Object>) -> JSValue {
        JSValue(JSValueUnion {
            bits: TAG_OBJECT | Gc::into_raw(v) as u64,
        })
    }
}

unsafe impl Trace for JSValue {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: Ctx) {
        if self.is_object() {
            unsafe { ctx.mark(self.into_object()) }
        } else if self.is_string() {
            unsafe { ctx.mark(self.into_string()) }
        }
    }
}

impl fmt::Debug for JSValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            match self.0.bits & TAG_MASK {
                TAG_BASE => match self.0.bits {
                    VALUE_TRUE => f.debug_tuple("JSValue::Bool").field(&true).finish(),
                    VALUE_FALSE => f.debug_tuple("JSValue::Bool").field(&false).finish(),
                    VALUE_NULL => f.debug_tuple("JSValue::Null").finish(),
                    VALUE_UNDEFINED => f.debug_tuple("JSValue::Undefined").finish(),
                    _ => todo!(),
                },
                TAG_STRING => f
                    .debug_tuple("JSValue::String")
                    .field(&self.into_string())
                    .finish(),
                TAG_OBJECT => {
                    let mut obj = f.debug_tuple("JSValue::Object");
                    obj.finish()
                }
                TAG_BIGINT => todo!(),
                TAG_SYMBOL => todo!(),
                TAG_FUNCTION => todo!(),
                TAG_INT => f
                    .debug_tuple("JSValue::Int")
                    .field(&self.into_int())
                    .finish(),
                _ => f
                    .debug_tuple("JSValue::Float")
                    .field(&self.into_float())
                    .finish(),
            }
        }
    }
}
