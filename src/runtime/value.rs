//! Javascript runtime value definition
//!
use crate::runtime::{object::ObjectRc, rc::RcCount, string::StringRc};
use std::{cmp, fmt, hash, mem, ops};

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

/// A NaN-tagged javascript value.
#[derive(Copy, Clone)]
pub union JSValue {
    float: f64,
    ptr: *mut (),
    int: i32,
    pub bits: u64,
}

impl cmp::Eq for JSValue {}
impl cmp::PartialEq<JSValue> for JSValue {
    fn eq(&self, other: &JSValue) -> bool {
        unsafe { self.bits == other.bits }
    }
}

impl hash::Hash for JSValue {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        state.write_u64(unsafe { self.bits })
    }
}

impl fmt::Debug for JSValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_float() {
            return f
                .debug_struct("JSValue")
                .field("float", &self.into_float())
                .finish();
        }
        match self.tag() {
            TAG_INT => f
                .debug_struct("JSValue")
                .field("int", &self.into_int())
                .finish(),
            TAG_BOOL => f
                .debug_struct("JSValue")
                .field("bool", &self.into_bool())
                .finish(),
            TAG_NULL => write!(f, "JSValue::Null"),
            TAG_UNDEFINED => write!(f, "JSValue::Undefined"),
            _ => f
                .debug_struct("JSValue")
                .field("ptr", &self.into_raw_ptr())
                .finish(),
        }
    }
}

impl fmt::Display for JSValue {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
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

impl From<bool> for JSValue {
    #[inline]
    fn from(v: bool) -> Self {
        JSValue {
            bits: TAG_BOOL + v as u64,
        }
    }
}

impl From<i32> for JSValue {
    #[inline]
    fn from(v: i32) -> Self {
        unsafe {
            let val: u32 = mem::transmute(v);
            JSValue {
                bits: TAG_INT | val as u64,
            }
        }
    }
}

impl From<ObjectRc> for JSValue {
    #[inline]
    fn from(val: ObjectRc) -> Self {
        unsafe {
            let ptr_val: u64 = mem::transmute(val.to_raw());
            JSValue {
                bits: (ptr_val & PTR_MASK) | TAG_OBJECT,
            }
        }
    }
}

impl From<f64> for JSValue {
    #[inline]
    fn from(v: f64) -> Self {
        JSValue { float: v }
    }
}

impl From<StringRc> for JSValue {
    #[inline]
    fn from(v: StringRc) -> Self {
        unsafe {
            let ptr_val: u64 = mem::transmute(v.to_raw());
            JSValue {
                bits: (ptr_val & PTR_MASK) | TAG_STRING,
            }
        }
    }
}

impl JSValue {
    /// Create a value representing the javascript value `undefined`
    #[inline]
    pub fn undefined() -> Self {
        JSValue {
            bits: TAG_UNDEFINED,
        }
    }

    /// Create a value representing the javascript value `null`
    #[inline]
    pub fn null() -> Self {
        JSValue { bits: TAG_NULL }
    }

    /// Returns the tag of the object.
    /// Note: value only contains a valid tag if the value does not contain a float.
    #[inline]
    pub fn tag(self) -> u64 {
        unsafe { self.bits & TAG_MASK }
    }

    /// Returns wether the tag of the value represents a boolean value
    #[inline]
    pub fn is_bool(self) -> bool {
        unsafe { self.bits & TAG_MASK == TAG_BOOL }
    }

    /// Returns wether the tag of the value represents a integer value
    #[inline]
    pub fn is_int(self) -> bool {
        unsafe { self.bits & TAG_MASK == TAG_INT }
    }

    /// Returns wether the value is a float and thus does not contain any other tags
    #[inline]
    pub fn is_float(self) -> bool {
        unsafe { self.bits <= MAX_DOUBLE }
    }

    /// Returns wether the tag of the value represents a object value
    #[inline]
    pub fn is_object(self) -> bool {
        unsafe { self.bits & TAG_MASK == TAG_OBJECT }
    }

    /// Returns wether the tag of the value represents a string value
    #[inline]
    pub fn is_string(self) -> bool {
        unsafe { self.bits & TAG_MASK == TAG_STRING }
    }

    /// Returns wether the tag of the value represents a undefined value
    #[inline]
    pub fn is_undefined(self) -> bool {
        unsafe { self.bits & TAG_MASK == TAG_UNDEFINED }
    }

    /// Returns wether the tag of the value represents a null value
    #[inline]
    pub fn is_null(self) -> bool {
        unsafe { self.bits & TAG_MASK == TAG_NULL }
    }

    /// Convert the value to a raw pointer
    #[inline]
    pub fn into_raw_ptr(self) -> *mut () {
        unsafe { mem::transmute(self.bits & PTR_MASK) }
    }

    /// Convert the value to a boolean
    /// # Panic
    /// Will panic in debug mode if ```self.is_bool()``` returns false
    #[inline]
    pub fn into_bool(self) -> bool {
        debug_assert!(self.is_bool());
        unsafe { (self.int & 0x1) == 1 }
    }

    /// Convert the value to a integer
    /// # Panic
    /// Will panic in debug mode if ```self.is_int()``` returns false
    #[inline]
    pub fn into_int(self) -> i32 {
        debug_assert!(self.is_int());
        unsafe { self.int }
    }

    /// Convert the value to a float
    /// # Panic
    /// Will panic in debug mode if ```self.is_float()``` returns false
    #[inline]
    pub fn into_float(self) -> f64 {
        debug_assert!(self.is_float());
        unsafe { self.float }
    }

    /// Converts the value into an object reference.
    /// # Panic
    /// Will panic in debug mode if ```self.is_object()``` returns false
    #[inline]
    pub fn into_object(self) -> ObjectRc {
        debug_assert!(self.is_object());
        ObjectRc::from_raw(self.into_raw_ptr())
    }

    /// Converts the value into an string reference.
    #[inline]
    pub fn into_string(self) -> StringRc {
        debug_assert!(self.is_string());
        StringRc::from_raw(self.into_raw_ptr())
    }

    /// Increment the reference count if there is one
    /// # Safety
    /// Calling this function on an improperly constructed value will result in undefined behaviour.
    /// If the tag of this value is the tag of a string of an object then the pointer contained
    /// must be a valid pointer
    #[inline]
    pub unsafe fn incr(self) {
        let tag = self.tag();
        if tag <= TAG_UNDEFINED {
            return;
        }
        RcCount::incr_raw(self.into_raw_ptr())
    }

    /// Create an independent jsvalue object
    /// # Safety
    /// Calling this function on an improperly constructed value will result in undefined behaviour.
    /// If the tag of this value is the tag of a string of an object then the pointer contained
    /// must be a valid pointer
    #[inline]
    pub unsafe fn deep_clone(&self) -> JSValue {
        let tag = self.tag();
        if tag <= TAG_UNDEFINED {
            return *self;
        }
        let ptr = self.into_raw_ptr();
        match tag {
            TAG_OBJECT => JSValue::from(ObjectRc::from_raw(ptr).deep_clone()),
            TAG_STRING => JSValue::from(StringRc::from_raw(ptr).deep_clone()),
            TAG_AVAILABLE_5 => panic!("found unused tag"),
            _ => unreachable!(),
        }
    }

    /// Decrement the reference count if there is one
    /// If the count is 0 drop the reverence.
    /// # Safety
    /// Calling this function on an improperly constructed value will result in undefined behaviour.
    /// If the tag of this value is the tag of a string of an object then the pointer contained
    /// must be a valid pointer
    #[inline]
    pub unsafe fn drop(self) {
        let tag = self.tag();
        if tag <= TAG_UNDEFINED {
            // Does not have a ref count.
            return;
        }
        match tag {
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
        unsafe { ((self.bits | 0xffff_ffff_0000_0000) >> 32) as u32 }
    }
    #[inline]
    pub fn low(self) -> u32 {
        unsafe { (self.bits | 0x0000_0000_ffff_ffff) as u32 }
    }
}
