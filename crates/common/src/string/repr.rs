use std::{
    alloc::Layout,
    mem::{ManuallyDrop, MaybeUninit},
    ops::BitOr,
};

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
    pub ptr: *mut T,
    pub len: usize,
}

impl<T: Copy> TaggedPtr<T> {
    pub fn from_slice(slice: &[T], flags: PtrFlags) -> Self {
        let this = Self::alloc(slice.len(), flags);

        unsafe {
            // SAFETY: ptr is just allocated to size len so is non overlapping.
            std::ptr::copy_nonoverlapping(slice.as_ptr(), this.ptr, slice.len());
        }
        this
    }

    pub fn alloc(len: usize, flags: PtrFlags) -> Self {
        // Should basically always be true on 64 bit platforms as they often can't even address
        // more than 2^48 bytes of memory.
        assert!(len < LEN_FLAGS, "string size exceeded maximum size");
        // Should not be able to overflow isize::MAX since it is derived from an existing
        // allocation
        let layout = std::alloc::Layout::array::<u8>(len).unwrap();
        unsafe {
            let ptr = std::alloc::alloc(layout).cast::<T>();
            assert_ne!(ptr, std::ptr::null_mut(), "allocation failed");

            Self {
                ptr,
                len: len | flags.0,
            }
        }
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
            std::alloc::dealloc(self.ptr.cast(), layout);
        }
    }
}

impl<T: Copy> Clone for TaggedPtr<T> {
    fn clone(&self) -> Self {
        let layout = std::alloc::Layout::array::<u8>(self.len()).unwrap();
        let ptr = unsafe {
            let ptr = std::alloc::alloc(layout).cast::<T>();
            assert_ne!(ptr, std::ptr::null_mut(), "allocation failed");
            std::ptr::copy_nonoverlapping(self.ptr, ptr, self.len());
            ptr
        };
        Self { ptr, len: self.len }
    }
}

pub enum CodeUnits<'a> {
    Ascii(&'a [u8]),
    Utf16(&'a [u16]),
}

pub union Repr {
    inline: [u8; INLINE_SIZE],
    ascii: ManuallyDrop<TaggedPtr<u8>>,
    utf16: ManuallyDrop<TaggedPtr<u16>>,
}

impl Repr {
    pub fn empty() -> Self {
        let mut inline = [MaybeUninit::uninit(); INLINE_SIZE];
        inline[INLINE_SIZE - 1].write(0u8);
        let inline = unsafe {
            std::mem::transmute::<[MaybeUninit<u8>; INLINE_SIZE], [u8; INLINE_SIZE]>(inline)
        };
        Self { inline }
    }

    /// Creates a new inline string from an existing string.
    /// Returns `None` if the string exceeds the maximum inline length or contains non ascii code
    /// points.
    pub const fn new_const(s: &str) -> Self {
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

    pub unsafe fn from_ascii(ascii: &[u8]) -> Self {
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

    pub fn as_bytes(&self) -> &[u8] {
        unsafe {
            if self.is_inline() {
                let len = self.inline_len() as usize;
                std::slice::from_raw_parts(self.inline.as_ptr(), len)
            } else if self.is_utf16() {
                std::slice::from_raw_parts(self.utf16.ptr.cast::<u8>(), self.heap_len() * 2)
            } else {
                std::slice::from_raw_parts(self.ascii.ptr, self.heap_len())
            }
        }
    }

    pub fn as_code_units(&self) -> CodeUnits {
        if self.is_inline() {
            let len = self.inline_len() as usize;
            unsafe {
                let slice = std::slice::from_raw_parts(self.inline.as_ptr(), len);
                CodeUnits::Ascii(slice)
            }
        } else if self.is_utf16() {
            let slice = unsafe { std::slice::from_raw_parts(self.utf16.ptr, self.heap_len()) };
            CodeUnits::Utf16(slice)
        } else {
            let slice = unsafe { std::slice::from_raw_parts(self.ascii.ptr, self.heap_len()) };
            CodeUnits::Ascii(slice)
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
