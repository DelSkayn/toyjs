use std::{fmt, mem};

const MAX_DOUBLE: u64 = (0xfff8_0000) << 32;
pub const TAG_PTR: u64 = (0xfff9_0000) << 32;
pub const TAG_STRING: u64 = (0xfffa_0000) << 32;
pub const TAG_INT: u64 = (0xfffb_0000) << 32;
pub const TAG_UNDEFINED: u64 = (0xfffc_0000) << 32;
pub const TAG_NULL: u64 = (0xfffd_0000) << 32;
const TAG_AVAILABLE_4: u64 = (0xfffe_0000) << 32;
const TAG_AVAILABLE_5: u64 = (0xffff_0000) << 32;

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

impl fmt::Display for JSValue {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        if self.is_int() {
            write!(w, "{{int:{}}}", self.into_int())?
        } else if self.is_float() {
            write!(w, "{{float:{}}}", self.into_float())?
        } else if self.is_ptr() {
            write!(w, "{{ptr:{:p}}}", self.into_ptr())?
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

impl JSValue {
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
    pub fn ptr(val: *mut ()) -> Self {
        unsafe {
            let ptr_val: u64 = mem::transmute(val);
            JSValue(JSValueUnion {
                bits: ptr_val | TAG_PTR,
            })
        }
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
        JSValue(JSValueUnion {
            bits: TAG_UNDEFINED,
        })
    }

    #[inline]
    pub fn is_int(self) -> bool {
        unsafe { self.0.bits & TAG_INT == TAG_INT }
    }

    #[inline]
    pub fn is_undefined(self) -> bool {
        unsafe { self.0.bits & TAG_UNDEFINED == TAG_UNDEFINED }
    }

    #[inline]
    pub fn is_null(self) -> bool {
        unsafe { self.0.bits & TAG_UNDEFINED == TAG_UNDEFINED }
    }

    #[inline]
    pub fn into_int(self) -> i32 {
        unsafe { self.0.int }
    }

    #[inline]
    pub fn is_ptr(self) -> bool {
        unsafe { self.0.bits & TAG_PTR == TAG_PTR }
    }

    #[inline]
    pub fn into_ptr(self) -> *mut () {
        unsafe { mem::transmute(self.0.bits & PTR_MASK) }
    }

    #[inline]
    pub fn is_float(self) -> bool {
        unsafe { self.0.bits < MAX_DOUBLE }
    }

    #[inline]
    pub fn into_float(self) -> f64 {
        unsafe { self.0.float }
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
