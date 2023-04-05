use std::ptr::NonNull;

/// A non-null pointer with a tag fit into the lower bits of the pointer available due to alignment of
/// memory.
pub struct TaggedPtr<T>(NonNull<T>);

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
        let value = ptr.as_ptr() as usize;
        // Guard against NonNull::dangeling().
        // A dangeling pointer can be less then mask.
        // If the tag would then be zero the resulting pointer would be zero which would violate
        // NonNull requirements. Making the pointer value 1 more then tag_mask ensures that it is
        // never zero.
        let value = value.max(Self::tag_mask() + 1);
        let ptr = (value | tag as usize) as *mut T;
        // SAFETY: Above guarentees that pointer is never zero
        let ptr = NonNull::new_unchecked(ptr);

        TaggedPtr(ptr)
    }

    #[inline]
    pub fn as_nonnull(self) -> NonNull<T> {
        // SAFETY: Implementation guarentees that as_ptr always returns a non null pointer.
        unsafe { NonNull::new_unchecked(self.as_ptr()) }
    }

    #[inline]
    pub fn as_ptr(self) -> *mut T {
        (self.0.as_ptr() as usize & !Self::tag_mask()) as *mut T
    }

    #[inline]
    pub fn tag(self) -> u8 {
        (self.0.as_ptr() as usize & Self::tag_mask()) as u8
    }

    /// # Safety
    /// The tag must fit in the amount of bits returned by [`TaggedPtr::tag_bits`].
    #[inline]
    pub unsafe fn set_tag(&mut self, tag: u8) {
        // This should always be larger then zero since the new implementation ensures that the
        // pointer is never null even when masked with the tag mask.
        let value = self.as_ptr() as usize | tag as usize;

        self.0 = NonNull::new_unchecked(value as *mut T)
    }

    pub fn split(self) -> (NonNull<T>, u8) {
        let ptr = self.as_nonnull();
        let tag = self.tag();
        (ptr, tag)
    }
}
