use core::slice;
use std::alloc::{self, Layout};

pub const LEN_FLAGS: usize = !(usize::MAX >> 2);

#[cfg(target_endian = "little")]
#[repr(C)]
#[derive(Clone, Copy)]
pub struct TaggedPtr<T: Copy> {
    pub ptr: *mut T,
    pub len: usize,
}

#[cfg(target_endian = "big")]
#[repr(C)]
#[derive(Clone, Copy)]
pub struct TaggedPtr<T: Copy> {
    pub len: usize,
    pub ptr: *mut T,
}

impl<T: Copy> TaggedPtr<T> {
    pub fn from_slice(slice: &[T]) -> Self {
        let this = Self::alloc(slice.len());

        unsafe {
            // SAFETY: ptr is just allocated to size len so is non overlapping.
            std::ptr::copy_nonoverlapping(slice.as_ptr(), this.ptr, slice.len());
        }
        this
    }

    pub fn alloc(len: usize) -> Self {
        // Should basically always be true on 64 bit platforms as they often can't even address
        // more than 2^48 bytes of memory.
        assert!(len < LEN_FLAGS, "string size exceeded maximum size");
        // Should not be able to overflow isize::MAX since it is derived from an existing
        // allocation
        let layout = Layout::array::<T>(len).unwrap();
        unsafe {
            let ptr = alloc::alloc(layout).cast::<T>();
            assert!(!ptr.is_null(), "allocation failed");

            Self { ptr, len }
        }
    }

    pub unsafe fn as_slice(&self) -> &[T] {
        slice::from_raw_parts_mut(self.ptr, self.len())
    }

    pub unsafe fn drop_in_place(&self) {
        let layout = Layout::array::<T>(self.len()).unwrap();
        unsafe {
            alloc::dealloc(self.ptr.cast(), layout);
        }
    }

    pub fn len(&self) -> usize {
        self.len & !LEN_FLAGS
    }

    pub fn clone_ptr(&self) -> Self {
        let layout = Layout::array::<u8>(self.len()).unwrap();
        let ptr = unsafe {
            let ptr = alloc::alloc(layout).cast::<T>();
            assert!(!ptr.is_null(), "allocation failed");
            std::ptr::copy_nonoverlapping(self.ptr, ptr, self.len());
            ptr
        };
        Self { ptr, len: self.len }
    }
}
