use crate::runtime::{object::ObjectRc, rc::RcCount, string::StringRc};
use std::{fmt, mem};

const MAX_DOUBLE: u64 = (0xfff8_0000) << 32;
pub const TAG_INT: u64 = (0xfff9_0000) << 32;
pub const TAG_BOOL: u64 = (0xfffa_0000) << 32;
pub const TAG_NULL: u64 = (0xfffb_0000) << 32;
pub const TAG_UNDEFINED: u64 = (0xfffc_0000) << 32;
pub const TAG_OBJECT: u64 = (0xfffd_0000) << 32;
pub const TAG_STRING: u64 = (0xfffe_0000) << 32;
pub const TAG_AVAILABLE_5: u64 = (0xffff_0000) << 32;
pub const TAG_MASK: u64 = 0xffff_0000 << 32;

const PTR_MASK: u64 = 0x0000_ffff_ffff_fff8;

#[derive(Copy, Clone)]
pub union JSValueUnion {
    pub float: f64,
    pub ptr: *mut (),
    pub int: i32,
    pub bits: u64,
}

#[derive(Copy, Clone)]
pub struct JSValue(pub JSValueUnion);

impl fmt::Debug for JSValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("JSValue")
            .field(unsafe { &self.0.bits })
            .finish()
    }
}

impl fmt::Display for JSValue {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        unsafe {
            if self.is_int() {
                write!(w, "{{int:{}}}", self.into_int())?
            } else if self.is_float() {
                write!(w, "{{float:{}}}", self.into_float())?
            } else if self.is_object() {
                write!(w, "{{object:{:p}}}", self.into_raw_ptr())?
            } else if self.is_string() {
                write!(w, "{{string:{:p}}}", self.into_raw_ptr())?
            } else if self.is_undefined() {
                write!(w, "{{undefined}}")?
            } else if self.is_null() {
                write!(w, "{{null}}")?
            } else {
                write!(w, "{{invalid js value!}}")?
            }
            Ok(())
        }
    }
}

impl JSValue {
    #[inline]
    pub fn bool(val: bool) -> Self {
        JSValue(JSValueUnion {
            bits: TAG_BOOL + val as u64,
        })
    }

    #[inline]
    pub fn int(val: i32) -> Self {
        unsafe {
            let val: u32 = mem::transmute(val);
            JSValue(JSValueUnion {
                bits: TAG_INT | val as u64,
            })
        }
    }

    #[inline]
    pub unsafe fn object(val: ObjectRc) -> Self {
        let ptr_val: u64 = mem::transmute(val.to_raw());
        JSValue(JSValueUnion {
            bits: (ptr_val & PTR_MASK) | TAG_OBJECT,
        })
    }

    #[inline]
    pub unsafe fn string(val: StringRc) -> Self {
        let ptr_val: u64 = mem::transmute(val.to_raw());
        JSValue(JSValueUnion {
            bits: (ptr_val & PTR_MASK) | TAG_STRING,
        })
    }

    #[inline]
    pub fn float(val: f64) -> Self {
        JSValue(JSValueUnion { float: val })
    }

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
    pub fn is_bool(self) -> bool {
        unsafe { self.0.bits & TAG_MASK == TAG_BOOL }
    }

    #[inline]
    pub fn is_int(self) -> bool {
        unsafe { self.0.bits & TAG_MASK == TAG_INT }
    }

    #[inline]
    pub fn is_undefined(self) -> bool {
        unsafe { self.0.bits & TAG_MASK == TAG_UNDEFINED }
    }

    #[inline]
    pub fn is_null(self) -> bool {
        unsafe { self.0.bits & TAG_MASK == TAG_NULL }
    }

    #[inline]
    pub fn into_bool(self) -> bool {
        unsafe { (self.0.int & 0x1) == 1 }
    }

    #[inline]
    pub fn into_int(self) -> i32 {
        unsafe { self.0.int }
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
    pub fn tag(self) -> u64 {
        unsafe { self.0.bits & TAG_MASK }
    }

    #[inline]
    pub unsafe fn into_raw_ptr(self) -> *mut () {
        mem::transmute(self.0.bits & PTR_MASK)
    }

    #[inline]
    pub unsafe fn into_object(self) -> ObjectRc {
        ObjectRc::from_raw(self.into_raw_ptr())
    }

    #[inline]
    pub unsafe fn into_string(self) -> StringRc {
        StringRc::from_raw(self.into_raw_ptr())
    }

    #[inline]
    pub unsafe fn is_float(self) -> bool {
        self.0.bits < MAX_DOUBLE
    }

    #[inline]
    pub unsafe fn into_float(self) -> f64 {
        self.0.float
    }

    /// Increment the reference count if there is one
    #[inline]
    pub unsafe fn incr(self) {
        let tag = self.tag();
        if tag <= TAG_UNDEFINED {
            return;
        }
        RcCount::incr_raw(self.into_raw_ptr())
    }

    /// Create an independent jsvalue object
    #[inline]
    pub unsafe fn clone(self) -> JSValue {
        let tag = self.tag();
        if tag <= TAG_UNDEFINED {
            return self;
        }
        let ptr = self.into_raw_ptr();
        match tag {
            TAG_OBJECT => JSValue::object(ObjectRc::from_raw(ptr).clone_obj()),
            TAG_STRING => JSValue::string(StringRc::from_raw(ptr).clone_obj()),
            TAG_AVAILABLE_5 => panic!("found unused tag"),
            _ => unreachable!(),
        }
    }

    #[inline]
    pub unsafe fn drop(self) {
        let mask = self.0.bits & TAG_MASK;
        if mask <= TAG_UNDEFINED {
            return;
        }
        match mask {
            TAG_OBJECT => {
                let ptr = self.into_raw_ptr();
                ObjectRc::from_raw(ptr).drop();
            }
            TAG_STRING => {
                let ptr = self.into_raw_ptr();
                StringRc::from_raw(ptr).drop();
            }
            TAG_AVAILABLE_5 => panic!("found unused tag"),
            _ => unreachable!(),
        }
    }

    #[inline]
    pub fn hi(self) -> u32 {
        unsafe { ((self.0.bits | 0xffff_ffff_0000_0000) >> 32) as u32 }
    }
    #[inline]
    pub fn low(self) -> u32 {
        unsafe { (self.0.bits | 0x0000_0000_ffff_ffff) as u32 }
    }
}
