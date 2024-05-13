//! Internal string representation
//!
//! assume 32bit
//!
//! LE:
//!    inline
//!    |c|c|c|c|c|c|c|l/f|
//!    ptr:
//!    |p|p|p|p|l|l|l|l/f|
//! BE:
//!    inline
//!    |l/f|c|c|c|c|c|c|c|
//!    ptr:
//!    |l/f|l|l|l|p|p|p|p|

use std::{
    fmt,
    hash::{Hash, Hasher},
    mem::MaybeUninit,
    ops::Range,
    slice,
};

use self::{flags::PtrFlag, tagged_ptr::TaggedPtr};
use super::{Ascii, Encoding, Utf16};

mod flags;
mod tagged_ptr;

const INLINE_SIZE: usize = std::mem::size_of::<TaggedPtr<u8>>();
const MAX_SIZE: usize = usize::MAX >> 2;

#[cfg(target_endian = "little")]
const FLAG_BYTE_INDEX: usize = INLINE_SIZE - 1;
#[cfg(target_endian = "big")]
const FLAG_BYTE_INDEX: usize = 0;

#[cfg(target_endian = "little")]
const INLINE_RANGE: Range<usize> = 0..(INLINE_SIZE - 1);
#[cfg(target_endian = "big")]
const INLINE_RANGE: Range<usize> = 1..INLINE_SIZE;

impl PartialEq for Repr {
    fn eq(&self, other: &Self) -> bool {
        self.as_code_units() == other.as_code_units()
    }
}

impl Eq for Repr {}

impl Hash for Repr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_code_units().hash(state);
    }
}

pub union Repr {
    inline: [u8; INLINE_SIZE],
    ascii: TaggedPtr<u8>,
    utf16: TaggedPtr<u16>,
}

impl Repr {
    pub const fn empty() -> Self {
        Self {
            inline: [0u8; INLINE_SIZE],
        }
    }

    /// Creates a new inline string from an existing string.
    /// Returns `None` if the string exceeds the maximum inline length or contains non ascii code
    /// points.
    pub const fn new_const(s: &str) -> Self {
        // Store inline
        if s.is_empty() {
            return Self::empty();
        }
        let mut inline = [0u8; INLINE_SIZE];
        let bytes = s.as_bytes();
        if bytes.len() >= INLINE_SIZE {
            panic!("to be inlined string to large length")
        }
        let len = bytes.len() as u8;

        let mut i = 0;
        let offset = (FLAG_BYTE_INDEX == 0) as u8;
        // While loop because const fn don't allow for loops.
        while i < len {
            let byte = bytes[i as usize];
            if !byte.is_ascii() {
                panic!("to be inlined string contains non ascii characters")
            }
            inline[(i + offset) as usize] = byte;
            i += 1;
        }
        inline[FLAG_BYTE_INDEX] = len;
        Repr { inline }
    }

    pub fn is_inline(&self) -> bool {
        self.flag_byte() & flags::PtrFlag::Heap as u8 == 0
    }

    pub fn is_utf16(&self) -> bool {
        self.flag_byte() & flags::PtrFlag::Utf16 as u8 == flags::PtrFlag::Utf16 as u8
    }

    pub fn inline_len(&self) -> u8 {
        self.flag_byte()
    }

    pub fn heap_len(&self) -> usize {
        unsafe { self.ascii.len() }
    }

    fn flag_byte(&self) -> u8 {
        unsafe { self.inline[FLAG_BYTE_INDEX] }
    }

    fn set_flag_byte(&mut self, v: u8) {
        unsafe {
            self.inline[FLAG_BYTE_INDEX] = v;
        }
    }

    pub fn from_ascii(ascii: &Ascii) -> Self {
        let ascii = ascii.units();
        if ascii.len() < INLINE_SIZE {
            let mut inline = [MaybeUninit::<u8>::uninit(); INLINE_SIZE];
            unsafe {
                std::ptr::copy_nonoverlapping(
                    ascii.as_ptr(),
                    inline[INLINE_RANGE].as_mut_ptr().cast::<u8>(),
                    ascii.len(),
                );
                inline[FLAG_BYTE_INDEX].write(ascii.len() as u8);
                Repr {
                    inline: std::mem::transmute::<[MaybeUninit<_>; INLINE_SIZE], [u8; INLINE_SIZE]>(
                        inline,
                    ),
                }
            }
        } else {
            let ascii = TaggedPtr::from_slice(ascii);
            let mut res = Repr { ascii };
            res.set_flag_byte(res.flag_byte() | PtrFlag::Heap as u8);
            res
        }
    }

    pub fn from_utf16(ascii: &Utf16) -> Self {
        let utf16 = TaggedPtr::from_slice(ascii.units());
        let mut res = Repr { utf16 };
        res.set_flag_byte(res.flag_byte() | PtrFlag::Heap as u8 | PtrFlag::Utf16 as u8);
        res
    }

    pub fn as_bytes(&self) -> &[u8] {
        unsafe {
            if self.is_inline() {
                let len = self.inline_len() as usize;
                slice::from_raw_parts(self.inline.as_ptr(), len)
            } else if self.is_utf16() {
                slice::from_raw_parts(self.utf16.ptr.cast::<u8>(), self.heap_len() * 2)
            } else {
                slice::from_raw_parts(self.ascii.ptr, self.heap_len())
            }
        }
    }

    pub fn as_code_units(&self) -> Encoding {
        if self.is_inline() {
            let len = self.inline_len() as usize;
            unsafe {
                let slice = slice::from_raw_parts(self.inline.as_ptr(), len);
                let ascii = Ascii::from_slice_unchecked(slice);
                Encoding::Ascii(ascii)
            }
        } else if self.is_utf16() {
            unsafe {
                let utf16 = Utf16::from_slice_unchecked(self.utf16.as_slice());
                Encoding::Utf16(utf16)
            }
        } else {
            unsafe {
                let ascii = Ascii::from_slice_unchecked(self.ascii.as_slice());
                Encoding::Ascii(ascii)
            }
        }
    }

    pub fn from_std_str(s: &str) -> Self {
        if s.is_ascii() {
            let ascii = unsafe { Ascii::from_slice_unchecked(s.as_bytes()) };
            Repr::from_ascii(ascii)
        } else {
            unsafe {
                let len = s.chars().map(|x| x.len_utf16()).sum();

                let utf16 = TaggedPtr::<u16>::alloc(len);

                for (idx, code_point) in s.encode_utf16().enumerate() {
                    utf16.ptr.add(idx).write(code_point);
                }

                let mut res = Repr { utf16 };
                res.set_flag_byte(res.flag_byte() | PtrFlag::Heap as u8 | PtrFlag::Utf16 as u8);
                res
            }
        }
    }
}

impl Clone for Repr {
    fn clone(&self) -> Self {
        unsafe {
            if self.is_inline() {
                Repr {
                    inline: self.inline,
                }
            } else if self.is_utf16() {
                Repr {
                    utf16: self.utf16.clone_ptr(),
                }
            } else {
                Repr {
                    ascii: self.ascii.clone_ptr(),
                }
            }
        }
    }
}

impl fmt::Debug for Repr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_inline() {
            f.debug_tuple("Repr::Inline")
                .field(unsafe { &self.inline })
                .finish()
        } else {
            match self.as_code_units() {
                Encoding::Ascii(ref x) => f.debug_tuple("Repr::Ascii").field(x).finish(),
                Encoding::Utf16(ref x) => f.debug_tuple("Repr::Utf16").field(x).finish(),
            }
        }
    }
}

impl Drop for Repr {
    fn drop(&mut self) {
        if !self.is_inline() {
            if self.is_utf16() {
                unsafe { self.utf16.drop_in_place() }
            } else {
                unsafe { self.ascii.drop_in_place() }
            }
        }
    }
}
