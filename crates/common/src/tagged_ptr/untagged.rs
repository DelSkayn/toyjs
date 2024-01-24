//! Untagged tagged pointer implementation for platforms which don't work well wit pointer tagging
//! like miri..

use std::ptr::NonNull;

/// A non-null pointer with a tag fit into the lower bits of the pointer available due to alignment of
/// memory.
pub struct TaggedPtr<T> {
    ptr: NonNull<T>,
    tag: u8,
}

impl<T> Clone for TaggedPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for TaggedPtr<T> {}

impl<T> TaggedPtr<T> {
    /// Returns the amount of bits within which the tag must fit.
    pub const fn tag_bits() -> u8 {
        let align = std::mem::align_of::<T>();
        let res = ((std::mem::size_of::<*mut T>() * 8) - align.leading_zeros() as usize)
            .saturating_sub(1);
        res as u8
    }

    /// Returns a bit mask within which the tag must fit.
    pub const fn tag_mask() -> usize {
        2_usize.pow(Self::tag_bits() as u32) - 1
    }

    /// # Safety
    ///
    /// The tag must fit in the amount of bits returned by [`TaggedPtr::tag_bits`].
    #[inline]
    pub unsafe fn new(ptr: NonNull<T>, tag: u8) -> Self {
        TaggedPtr { ptr, tag }
    }

    #[inline]
    pub fn as_nonnull(self) -> NonNull<T> {
        self.ptr
    }

    #[inline]
    pub fn as_ptr(self) -> *mut T {
        self.ptr.as_ptr()
    }

    #[inline]
    pub fn tag(self) -> u8 {
        self.tag
    }

    /// # Safety
    /// The tag must fit in the amount of bits returned by [`TaggedPtr::tag_bits`].
    #[inline]
    pub unsafe fn set_tag(&mut self, tag: u8) {
        self.tag = tag;
    }

    pub fn split(self) -> (NonNull<T>, u8) {
        (self.ptr, self.tag)
    }
}
