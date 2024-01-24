use core::fmt;
use std::{
    alloc::{self, Layout},
    hash::{Hash, Hasher},
    mem::{ManuallyDrop, MaybeUninit},
    ops::BitOr,
    ptr::NonNull,
    slice,
};

use super::encoding::{Ascii, Encoding, Utf16};

/// TODO: 32bit platforms.

/// The flag which indicates that the string is allocated on the heap.
const ON_HEAP: usize = 1 << ((std::mem::size_of::<usize>() * 8) - 2);
/// The flag which indicates that the string contains utf16 code points.
const UTF16: usize = 1 << ((std::mem::size_of::<usize>() * 8) - 1);
/// A mask of both flags.
const LEN_FLAGS: usize = ON_HEAP | UTF16;

// This enum is portable since the value is also dependent on the size of usize
#[allow(clippy::enum_clike_unportable_variant)]
#[repr(usize)]
pub enum PtrFlag {
    Heap = 1 << ((std::mem::size_of::<usize>() * 8) - 2),
    Utf16 = 1 << ((std::mem::size_of::<usize>() * 8) - 1),
}

pub struct PtrFlags(usize);

impl PtrFlags {
    pub fn none() -> Self {
        Self(0)
    }
}

impl From<PtrFlag> for PtrFlags {
    fn from(value: PtrFlag) -> Self {
        Self(value as usize)
    }
}

impl BitOr<PtrFlag> for PtrFlag {
    type Output = PtrFlags;

    fn bitor(self, rhs: PtrFlag) -> Self::Output {
        PtrFlags::from(self) | PtrFlags::from(rhs)
    }
}

impl BitOr<PtrFlags> for PtrFlags {
    type Output = PtrFlags;

    fn bitor(self, rhs: PtrFlags) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

/// The size of the inline buffer.
const INLINE_SIZE: usize = std::mem::size_of::<TaggedPtr<u8>>();

#[repr(C)]
pub struct TaggedPtr<T: Copy> {
    pub ptr: NonNull<T>,
    pub len: usize,
}

impl<T: Copy> TaggedPtr<T> {
    pub fn from_slice(slice: &[T], flags: PtrFlags) -> Self {
        let this = Self::alloc(slice.len(), flags);

        unsafe {
            // SAFETY: ptr is just allocated to size len so is non overlapping.
            std::ptr::copy_nonoverlapping(slice.as_ptr(), this.ptr.as_ptr(), slice.len());
        }
        this
    }

    pub fn alloc(len: usize, flags: PtrFlags) -> Self {
        // Should basically always be true on 64 bit platforms as they often can't even address
        // more than 2^48 bytes of memory.
        assert!(len < LEN_FLAGS, "string size exceeded maximum size");
        // Should not be able to overflow isize::MAX since it is derived from an existing
        // allocation
        let layout = alloc::Layout::array::<T>(len).unwrap();
        unsafe {
            let ptr = alloc::alloc(layout).cast::<T>();
            let ptr = NonNull::new(ptr).expect("allocation failed");

            Self {
                ptr,
                len: len | flags.0,
            }
        }
    }

    unsafe fn as_slice(&self) -> &[T] {
        slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len())
    }

    fn len(&self) -> usize {
        self.len & !LEN_FLAGS
    }

    fn flags(&self) -> PtrFlags {
        PtrFlags(self.len & LEN_FLAGS)
    }
}

impl<T: Copy> Drop for TaggedPtr<T> {
    fn drop(&mut self) {
        let layout = Layout::array::<T>(self.len & !LEN_FLAGS).unwrap();
        unsafe {
            alloc::dealloc(self.ptr.as_ptr().cast(), layout);
        }
    }
}

impl<T: Copy> Clone for TaggedPtr<T> {
    fn clone(&self) -> Self {
        let layout = alloc::Layout::array::<u8>(self.len()).unwrap();
        let ptr = unsafe {
            let ptr = alloc::alloc(layout).cast::<T>();
            let ptr = NonNull::new(ptr).expect("allocation failed");
            std::ptr::copy_nonoverlapping(self.ptr.as_ptr(), ptr.as_ptr(), self.len());
            ptr
        };
        Self { ptr, len: self.len }
    }
}

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
    ascii: ManuallyDrop<TaggedPtr<u8>>,
    utf16: ManuallyDrop<TaggedPtr<u16>>,
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
                    utf16: self.utf16.clone(),
                }
            } else {
                Repr {
                    ascii: self.ascii.clone(),
                }
            }
        }
    }
}

impl Repr {
    pub const fn empty() -> Self {
        let mut inline = [0u8; INLINE_SIZE];
        inline[0] = 1;
        Self { inline }
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
        Repr { inline }
    }

    pub unsafe fn from_tagged_ptr_utf16(ptr: TaggedPtr<u16>) -> Self {
        Repr {
            utf16: ManuallyDrop::new(ptr),
        }
    }

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

    pub fn from_ascii(ascii: &Ascii) -> Self {
        let ascii = ascii.units();
        if ascii.len() < INLINE_SIZE {
            let mut inline = [MaybeUninit::<u8>::uninit(); INLINE_SIZE];
            unsafe {
                std::ptr::copy_nonoverlapping(
                    ascii.as_ptr(),
                    inline.as_mut_ptr().cast::<u8>(),
                    ascii.len(),
                );
                inline[INLINE_SIZE - 1].write(ascii.len() as u8);
                Repr {
                    inline: std::mem::transmute(inline),
                }
            }
        } else {
            let ascii = TaggedPtr::from_slice(ascii, PtrFlag::Heap.into());
            Repr {
                ascii: ManuallyDrop::new(ascii),
            }
        }
    }

    pub fn from_utf16(utf16: &Utf16) -> Self {
        let ascii = TaggedPtr::from_slice(utf16.units(), PtrFlag::Heap | PtrFlag::Utf16);
        Repr {
            utf16: ManuallyDrop::new(ascii),
        }
    }

    pub fn as_bytes(&self) -> &[u8] {
        unsafe {
            if self.is_inline() {
                let len = self.inline_len() as usize;
                slice::from_raw_parts(self.inline.as_ptr(), len)
            } else if self.is_utf16() {
                slice::from_raw_parts(self.utf16.ptr.as_ptr().cast::<u8>(), self.heap_len() * 2)
            } else {
                slice::from_raw_parts(self.ascii.ptr.as_ptr(), self.heap_len())
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
                unsafe { ManuallyDrop::drop(&mut self.utf16) }
            } else {
                unsafe { ManuallyDrop::drop(&mut self.ascii) }
            }
        }
    }
}
