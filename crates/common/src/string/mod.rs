//! Implementation of immutable utf16 with optimization for short strings.

use std::{
    borrow::Cow,
    fmt::{self, Write},
};

use crate::unicode::Utf16Ext;

mod repr;
pub use repr::CodeUnits;
use repr::{PtrFlag, Repr, TaggedPtr};

/// An immutable string data type for a utf-16 string.
/// Can store strings as ascii if they don't contain non ascii code points.
/// Will store small strings inline, without allocating.
/// Small size of only 2 pointer widths.
#[repr(transparent)]
pub struct String(Repr);

impl String {
    /// Returns the empty string
    /// Doesn't allocate
    pub fn empty() -> Self {
        String(Repr::empty())
    }

    /// Creates a new inline string from an existing string.
    /// Returns `None` if the string exceeds the maximum inline length or contains non ascii code
    /// points.
    pub const fn new_const(s: &str) -> Self {
        String(Repr::new_const(s))
    }

    /// Returns the number of code points in the string.
    #[must_use]
    pub fn len(&self) -> usize {
        if self.0.is_inline() {
            self.0.inline_len() as usize
        } else {
            self.0.heap_len()
        }
    }

    /// Returns the bytes of the string.
    /// # Note
    /// The bytes can be formated as an ascii string if it doesn't contain any non ascii code
    /// points. Use [`String::is_ascii`] to determine the format.
    #[inline]
    pub fn bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }

    /// Returns whether the string is represented as an ascii string
    #[inline]
    pub fn is_ascii(&self) -> bool {
        !self.0.is_utf16()
    }

    /// Returns whether the string is inlined
    #[inline]
    pub fn is_inline(&self) -> bool {
        self.0.is_inline()
    }

    #[inline]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn code_units(&self) -> CodeUnits {
        self.0.as_code_units()
    }

    /// Returns an iterator over the chars of the string
    pub fn chars(&self) -> Chars {
        Chars(self.code_units())
    }

    /// Convert the string into a rust string,
    /// This is O(1) when the string is ascii but allocates and is O(n) when the string is
    /// represented as utf16.
    pub fn as_str(&self) -> Cow<str> {
        match self.code_units() {
            CodeUnits::Ascii(x) => {
                let str = unsafe { std::str::from_utf8_unchecked(x) };
                Cow::Borrowed(str)
            }
            CodeUnits::Utf16(x) => {
                let str = Chars(CodeUnits::Utf16(x)).collect::<std::string::String>();
                Cow::Owned(str)
            }
        }
    }
}

pub struct Chars<'a>(CodeUnits<'a>);

impl<'a> Iterator for Chars<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0 {
            CodeUnits::Ascii(ref mut x) => {
                let (ch, rest) = x.split_first()?;
                *x = rest;
                Some(*ch as char)
            }
            CodeUnits::Utf16(ref mut x) => {
                let (first, rest) = x.split_first()?;
                // Is it a surrogate
                if first.utf16_surrogate() {
                    let (second, rest) = rest
                        .split_first()
                        .expect("orphan utf16 surrogate in string");
                    *x = rest;

                    let c = first.utf16_extend(*second);
                    Some(unsafe { char::from_u32_unchecked(c) })
                } else {
                    *x = rest;
                    Some(unsafe { char::from_u32_unchecked(*first as u32) })
                }
            }
        }
    }
}

impl fmt::Display for String {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.chars().try_for_each(|x| f.write_char(x))
    }
}

impl From<std::string::String> for String {
    fn from(value: std::string::String) -> Self {
        Self::from(value.as_str())
    }
}

impl From<&str> for String {
    fn from(value: &str) -> Self {
        if value.is_ascii() {
            String(unsafe { Repr::from_ascii(value.as_bytes()) })
        } else {
            unsafe {
                let len = value.chars().map(|x| x.len_utf16()).sum();

                let ptr = TaggedPtr::<u16>::alloc(len, PtrFlag::Utf16 | PtrFlag::Heap);

                for (idx, code_point) in value.encode_utf16().enumerate() {
                    ptr.ptr.add(idx).write(code_point);
                }

                String(Repr::from_tagged_ptr_utf16(ptr))
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic_empty() {
        let string = String::empty();
        assert!(string.is_ascii());
        assert!(string.is_inline());
        assert!(string.is_empty());
        assert_eq!(string.len(), 0);
        assert_eq!(string.bytes().len(), 0);
        assert_eq!(format!("{}", string), "");
        assert_eq!(string.as_str(), "");
    }

    #[test]
    fn basic_inline() {
        let string = String::from("hello world!!!!");
        assert!(string.is_ascii());
        assert!(string.is_inline());
        assert!(!string.is_empty());
        assert_eq!(string.len(), 15);
        assert_eq!(string.bytes().len(), 15);
        assert_eq!(format!("{}", string), "hello world!!!!");
        assert_eq!(string.as_str(), "hello world!!!!");
    }

    #[test]
    fn basic_heap() {
        let string = String::from("helloooo worlddddd!!!!!");
        assert!(string.is_ascii());
        assert!(!string.is_inline());
        assert!(!string.is_empty());
        assert_eq!(string.len(), 23);
        assert_eq!(string.bytes().len(), 23);
        assert_eq!(format!("{}", string), "helloooo worlddddd!!!!!");
        assert_eq!(string.as_str(), "helloooo worlddddd!!!!!");
    }

    #[test]
    fn basic_utf16() {
        let string = String::from("❤️❤️❤️❤️");
        assert!(!string.is_inline());
        assert!(!string.is_ascii());
        assert!(!string.is_empty());
        assert_eq!(string.len(), 8);
        assert_eq!(string.bytes().len(), 16);
        assert_eq!(format!("{}", string), "❤️❤️❤️❤️");
        assert_eq!(string.as_str(), "❤️❤️❤️❤️");
    }

    #[test]
    fn const_inline() {
        const STRING: String = String::new_const("hello world!!!!");
        assert!(STRING.is_ascii());
        assert!(STRING.is_inline());
        assert!(!STRING.is_empty());
        assert_eq!(STRING.len(), 15);
        assert_eq!(STRING.bytes().len(), 15);
        assert_eq!(format!("{}", STRING), "hello world!!!!");
        assert_eq!(STRING.as_str(), "hello world!!!!");
    }
}
