use crate::{
    gc::{Ctx, Gc, Trace},
    object::Object,
};
use std::{cell::RefCell, cmp, fmt, hash, marker::PhantomData, mem, rc::Rc};

const MAX_DOUBLE: u64 = (0xfff8_0000) << 32;
pub const TAG_INT: u64 = (0xfff9_0000) << 32;
pub const TAG_BOOL: u64 = (0xfffa_0000) << 32;
pub const TAG_NULL: u64 = (0xfffb_0000) << 32;
pub const TAG_UNDEFINED: u64 = (0xfffc_0000) << 32;
pub const TAG_OBJECT: u64 = 0xfffd_0000_0000_0000;
pub const TAG_STRING: u64 = (0xfffe_0000) << 32;
pub const TAG_AVAILABLE_5: u64 = (0xffff_0000) << 32;
pub const TAG_MASK: u64 = 0xffff_0000 << 32;

const VALUE_TRUE: u64 = TAG_BOOL | 1;
const VALUE_FALSE: u64 = TAG_BOOL | 0;

const PTR_MASK: u64 = 0x0000_ffff_ffff_ffff;

/// A NaN-tagged javascript value.
#[derive(Copy, Clone)]
pub union JSValueUnion {
    float: f64,
    int: i32,
    boolean: bool,
    pub bits: u64,
}

impl cmp::Eq for JSValueUnion {}
impl cmp::PartialEq<JSValueUnion> for JSValueUnion {
    fn eq(&self, other: &JSValueUnion) -> bool {
        unsafe { self.bits == other.bits }
    }
}

impl hash::Hash for JSValueUnion {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        state.write_u64(unsafe { self.bits })
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct JSValue(JSValueUnion);

impl Default for JSValue {
    fn default() -> Self {
        JSValue::undefined()
    }
}

impl JSValue {
    #[inline]
    pub fn undefined() -> Self {
        JSValue(JSValueUnion {
            bits: TAG_UNDEFINED,
        })
    }

    #[inline]
    pub fn null() -> Self {
        JSValue(JSValueUnion { bits: TAG_NULL })
    }

    #[inline]
    pub fn tag(self) -> u64 {
        unsafe { self.0.bits & TAG_MASK }
    }

    #[inline]
    pub unsafe fn from_bits(bits: u64) -> Self {
        JSValue(JSValueUnion { bits })
    }

    #[inline]
    pub fn bits(self) -> u64 {
        unsafe { self.0.bits }
    }

    #[inline]
    pub fn is_null(self) -> bool {
        unsafe { self.0.bits == TAG_NULL }
    }

    #[inline]
    pub fn is_undefined(self) -> bool {
        unsafe { self.0.bits == TAG_UNDEFINED }
    }

    #[inline]
    pub fn is_string(self) -> bool {
        unsafe { self.0.bits & TAG_MASK == TAG_STRING }
    }

    #[inline]
    pub fn is_object(self) -> bool {
        unsafe { self.0.bits & TAG_MASK == TAG_OBJECT }
    }

    #[inline]
    pub fn is_int(self) -> bool {
        unsafe { self.0.bits & TAG_MASK == TAG_INT }
    }

    #[inline]
    pub fn is_bool(self) -> bool {
        unsafe { self.0.bits & TAG_MASK == TAG_BOOL }
    }

    #[inline]
    pub fn is_float(self) -> bool {
        unsafe { self.0.bits < MAX_DOUBLE }
    }

    /// # Safety
    ///
    /// Calling on a JSValue which is not tagged as a bool is undefined behaviour.
    ///
    /// If the functions is called on a JSValue which was not a boolean the
    /// resulting bool might be a invalid variant which can cause the compiler to emit invalid
    /// optimizations and thus is undefined behaviour.
    #[inline]
    pub unsafe fn into_bool(self) -> bool {
        // Not equal so to be closer to a c like bool
        debug_assert!(self.0.bits & TAG_MASK == TAG_BOOL);
        let res = self.0.boolean;
        mem::forget(self);
        res
    }

    #[inline]
    pub fn into_float(self) -> f64 {
        unsafe {
            debug_assert!(self.0.bits & TAG_MASK <= MAX_DOUBLE);
            let res = self.0.float;
            mem::forget(self);
            res
        }
    }

    #[inline]
    pub fn into_int(self) -> i32 {
        unsafe {
            debug_assert!(self.0.bits & TAG_MASK == TAG_INT);
            let res = self.0.int;
            mem::forget(self);
            res
        }
    }

    #[inline]
    pub unsafe fn into_string(self) -> Gc<String> {
        debug_assert!(self.0.bits & TAG_MASK == TAG_STRING);
        Gc::from_raw((self.0.bits & PTR_MASK) as *mut ())
    }

    #[inline]
    pub unsafe fn into_object(self) -> Gc<Object> {
        debug_assert!(self.0.bits & TAG_MASK == TAG_OBJECT);
        Gc::from_raw((self.0.bits & PTR_MASK) as *mut ())
    }
}

impl From<bool> for JSValue {
    #[inline]
    fn from(b: bool) -> JSValue {
        if b {
            JSValue(JSValueUnion { bits: VALUE_TRUE })
        } else {
            JSValue(JSValueUnion { bits: VALUE_FALSE })
        }
    }
}

impl From<i32> for JSValue {
    #[inline]
    fn from(v: i32) -> JSValue {
        JSValue(JSValueUnion {
            bits: TAG_INT | (v as u32) as u64,
        })
    }
}

impl From<f64> for JSValue {
    #[inline]
    fn from(v: f64) -> JSValue {
        JSValue(JSValueUnion { float: v })
    }
}

impl From<Gc<String>> for JSValue {
    #[inline]
    fn from(v: Gc<String>) -> JSValue {
        let ptr = Gc::into_raw(v);
        let val = JSValue(JSValueUnion {
            bits: TAG_STRING | (ptr as usize as u64 & PTR_MASK),
        });
        val
    }
}

impl From<Gc<Object>> for JSValue {
    #[inline]
    fn from(v: Gc<Object>) -> Self {
        let ptr = Gc::into_raw(v);
        let val = JSValue(JSValueUnion {
            bits: TAG_OBJECT | (ptr as usize as u64 & PTR_MASK),
        });
        val
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
                TAG_STRING => f
                    .debug_tuple("JSValue::String")
                    .field(&self.into_string())
                    .finish(),
                TAG_OBJECT => {
                    let mut obj = f.debug_tuple("JSValue::Object");
                    obj.finish()
                }
                TAG_BOOL => f
                    .debug_tuple("JSValue::Bool")
                    .field(&self.into_bool())
                    .finish(),
                TAG_INT => f
                    .debug_tuple("JSValue::Int")
                    .field(&self.into_int())
                    .finish(),
                TAG_NULL => f.debug_tuple("JSValue::Null").finish(),
                TAG_UNDEFINED => f.debug_tuple("JSValue::Undefined").finish(),
                TAG_AVAILABLE_5 => panic!(),
                _ => f
                    .debug_tuple("JSValue::Float")
                    .field(&self.into_float())
                    .finish(),
            }
        }
    }
}
