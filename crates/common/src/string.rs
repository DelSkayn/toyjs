//! Implementation of immutable utf16 with optimization for short strings.

use std::{
    alloc::Layout,
    borrow::Cow,
    fmt::{self, Write},
    mem::ManuallyDrop,
};

use crate::unicode::Utf16Ext;

/// TODO: 32bit platforms.

/// The flag which indicates that the string is allocated on the heap.
const ON_HEAP: usize = 1 << ((std::mem::size_of::<usize>() * 8) - 2);
/// The flag which indicates that the string contains utf16 code points.
const UTF16: usize = 1 << ((std::mem::size_of::<usize>() * 8) - 1);
/// A mask of both flags.
const LEN_FLAGS: usize = ON_HEAP | UTF16;
/// The size of the inline buffer.
const INLINE_SIZE: usize = std::mem::size_of::<StringBox<u8>>();

union InnerString {
    inline: [u8; std::mem::size_of::<StringBox<u8>>()],
    ascii: ManuallyDrop<StringBox<u8>>,
    utf16: ManuallyDrop<StringBox<u16>>,
}

impl InnerString {
    pub fn is_inline(&self) -> bool {
        unsafe { self.utf16.len & ON_HEAP != ON_HEAP }
    }

    pub fn is_utf16(&self) -> bool {
        unsafe { self.utf16.len & UTF16 == UTF16 }
    }

    pub fn inline_len(&self) -> u8 {
        unsafe { self.inline[INLINE_SIZE - 1] }
    }

    pub fn heap_len(&self) -> usize {
        unsafe { self.ascii.len & !LEN_FLAGS }
    }
}

#[repr(C)]
pub struct StringBox<T> {
    ptr: *mut T,
    len: usize,
}

impl<T> Drop for StringBox<T> {
    fn drop(&mut self) {
        let layout = Layout::array::<T>(self.len & !LEN_FLAGS).unwrap();
        unsafe {
            std::alloc::dealloc(self.ptr.cast(), layout);
        }
    }
}

/// An immutable string data type for a utf-16 string.
/// Can store strings as ascii if they don't contain non ascii code points.
/// Will store small strings inline, without allocating.
/// Small size of only 2 pointer widths.
#[repr(transparent)]
pub struct String(InnerString);

impl fmt::Debug for String {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("String")
            .field(unsafe { &self.0.inline })
            .finish()
    }
}

impl String {
    /// Returns the empty string
    /// Doesn't allocate
    pub fn empty() -> Self {
        String(InnerString {
            inline: [0u8; INLINE_SIZE],
        })
    }

    /// Creates a new inline string from an existing string.
    /// Returns `None` if the string exceeds the maximum inline length or contains non ascii code
    /// points.
    pub const fn new_inline(s: &str) -> Self {
        // Store inline
        let mut inline = [0u8; INLINE_SIZE];
        let bytes = s.as_bytes();
        if bytes.len() >= INLINE_SIZE {
            panic!("to be inlined string to large length")
        }
        let len = bytes.len() as u8;

        let mut i = 0;
        // While loop because const fn don't allow for loops.
        while i < len {
            let byte = bytes[i as usize];
            if !byte.is_ascii() {
                panic!("to be inlined string contains non ascii characters")
            }
            inline[i as usize] = byte;
            i += 1;
        }
        inline[INLINE_SIZE - 1] = len;
        String(InnerString { inline })
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
    pub fn bytes(&self) -> &[u8] {
        unsafe {
            if self.0.is_inline() {
                let len = self.0.inline_len() as usize;
                let ptr = &self.0.inline as *const u8;
                std::slice::from_raw_parts(ptr, len)
            } else if self.is_ascii() {
                std::slice::from_raw_parts(self.0.ascii.ptr, self.0.heap_len())
            } else {
                std::slice::from_raw_parts(self.0.utf16.ptr.cast::<u8>(), self.0.heap_len() * 2)
            }
        }
    }

    /// Returns whether the string is represented as an ascii string
    pub fn is_ascii(&self) -> bool {
        !self.0.is_utf16()
    }

    /// Returns whether the string is inlined
    pub fn is_inline(&self) -> bool {
        self.0.is_inline()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        unsafe { self.0.ascii.len == 0 }
    }

    /// Returns an iterator over the chars of the string
    pub fn chars(&self) -> Chars {
        if self.is_ascii() {
            Chars::Ascii(self.bytes())
        } else {
            unsafe {
                let slice = std::slice::from_raw_parts(self.0.utf16.ptr, self.0.heap_len());
                Chars::Utf16(slice)
            }
        }
    }

    /// Convert the string into a rust string,
    /// This is O(1) when the string is ascii but allocates and is O(n) when the string is
    /// represented as utf16.
    pub fn as_str(&self) -> Cow<str> {
        if self.is_ascii() {
            let str = unsafe {
                let bytes = if self.0.is_inline() {
                    let len = self.0.inline_len() as usize;
                    let ptr = &self.0.inline as *const u8;
                    std::slice::from_raw_parts(ptr, len)
                } else {
                    std::slice::from_raw_parts(self.0.ascii.ptr, self.0.heap_len())
                };
                std::str::from_utf8_unchecked(bytes)
            };
            Cow::Borrowed(str)
        } else {
            Cow::Owned(self.chars().collect::<std::string::String>())
        }
    }
}

pub enum Chars<'a> {
    Ascii(&'a [u8]),
    Utf16(&'a [u16]),
}

impl<'a> Iterator for Chars<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        match *self {
            Chars::Ascii(ref mut x) => {
                let (ch, rest) = x.split_first()?;
                *x = rest;
                Some(*ch as char)
            }
            Chars::Utf16(ref mut x) => {
                let (first, rest) = x.split_first()?;
                // Is it a surrogate
                if first.utf16_surrogate() {
                    let (second, rest) = rest.split_first()?;
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

impl Drop for String {
    fn drop(&mut self) {
        if !self.is_inline() {
            unsafe {
                if self.0.is_utf16() {
                    ManuallyDrop::drop(&mut self.0.utf16);
                } else {
                    ManuallyDrop::drop(&mut self.0.ascii);
                }
            }
        }
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
            if value.len() < INLINE_SIZE {
                // Store inline
                let mut res = [0u8; INLINE_SIZE];

                for (idx, v) in value.bytes().enumerate() {
                    res[idx] = v;
                }
                // Store the length in the last byte of the array.
                // These bytes should always be zero as length should generally be less then
                res[INLINE_SIZE - 1] = value.len() as u8;

                String(InnerString { inline: res })
            } else {
                unsafe {
                    let len = value.len();
                    // Should not be able to overflow isize::MAX since it is derived from an existing
                    // allocation
                    let layout = std::alloc::Layout::array::<u8>(len).unwrap();
                    let ptr = std::alloc::alloc(layout);
                    assert_ne!(ptr, std::ptr::null_mut(), "allocation failed");

                    // SAFETY: ptr is just allocated to size len so is non overlapping.
                    std::ptr::copy_nonoverlapping(value.as_ptr(), ptr, len);

                    String(InnerString {
                        ascii: ManuallyDrop::new(StringBox {
                            ptr,
                            len: len | ON_HEAP,
                        }),
                    })
                }
            }
        } else {
            unsafe {
                let len = value.chars().map(|x| x.len_utf16()).sum();
                // Should not be able to overflow isize::MAX since it is derived from an existing
                // allocation
                let layout = std::alloc::Layout::array::<u16>(len).unwrap();
                let ptr = std::alloc::alloc(layout).cast::<u16>();
                assert_ne!(ptr, std::ptr::null_mut(), "allocation failed");

                for (idx, code_point) in value.encode_utf16().enumerate() {
                    ptr.add(idx).write(code_point);
                }

                String(InnerString {
                    utf16: ManuallyDrop::new(StringBox {
                        ptr,
                        len: len | UTF16 | ON_HEAP,
                    }),
                })
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
        const STRING: String = String::new_inline("hello world!!!!");
        assert!(STRING.is_ascii());
        assert!(STRING.is_inline());
        assert!(!STRING.is_empty());
        assert_eq!(STRING.len(), 15);
        assert_eq!(STRING.bytes().len(), 15);
        assert_eq!(format!("{}", STRING), "hello world!!!!");
        assert_eq!(STRING.as_str(), "hello world!!!!");
    }
}
