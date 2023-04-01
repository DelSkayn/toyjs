use std::{
    alloc::{Allocator, Global},
    cell::UnsafeCell,
    fmt,
    slice::Iter,
};

/// A vector which allows push and poping from a shared ref but does not allow
/// obtaining references to internal values.
pub struct CellVec<T, A: Allocator = Global>(UnsafeCell<Vec<T, A>>);

impl<T: Clone, A: Allocator + Clone> Clone for CellVec<T, A> {
    fn clone(&self) -> Self {
        unsafe { Self(UnsafeCell::new((*self.0.get()).clone())) }
    }
}

impl<T: fmt::Debug> fmt::Debug for CellVec<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe { (*self.0.get()).fmt(f) }
    }
}

/// It is save to send the vector across threads since there can be no references to internal
/// values.
unsafe impl<T: Send, A: Allocator + Send> Send for CellVec<T, A> {}

impl<T> CellVec<T, Global> {
    pub fn new() -> Self {
        Self::new_in(Global)
    }

    pub fn with_capactity(capacity: usize) -> Self {
        CellVec(UnsafeCell::new(Vec::with_capacity(capacity)))
    }
}

impl<T, A: Allocator> CellVec<T, A> {
    #[inline]
    pub fn new_in(alloc: A) -> Self {
        CellVec(UnsafeCell::new(Vec::new_in(alloc)))
    }

    #[inline]
    pub fn into_inner(self) -> Vec<T, A> {
        self.0.into_inner()
    }

    #[inline]
    pub fn push(&self, value: T) {
        unsafe { (*self.0.get()).push(value) }
    }

    #[inline]
    pub fn pop(&self) -> Option<T> {
        unsafe { (*self.0.get()).pop() }
    }

    #[inline]
    pub fn len(&self) -> usize {
        unsafe { (*self.0.get()).len() }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    pub fn clear(&self) {
        unsafe { (*self.0.get()).clear() }
    }

    pub fn as_mut_ptr(&self) -> *mut T {
        unsafe { (*self.0.get()).as_mut_ptr() }
    }

    /// # Safety
    ///
    /// None of the other functions can be called while holding on to the iterator.
    #[inline]
    pub unsafe fn unsafe_iter(&self) -> Iter<T> {
        (*self.0.get()).iter()
    }

    #[inline]
    pub fn set(&self, idx: usize, value: T) {
        unsafe { (*self.0.get())[idx] = value }
    }

    /// # Safety
    ///
    /// Idx must be smaller the length of the vector.
    #[inline]
    pub unsafe fn set_unchecked(&self, idx: usize, value: T) {
        *(*self.0.get()).get_unchecked_mut(idx) = value
    }
}

impl<T: Clone, A: Allocator> CellVec<T, A> {
    #[inline]
    pub fn get(&self, idx: usize) -> Option<T> {
        unsafe { (*self.0.get()).get(idx).cloned() }
    }

    /// # Safety
    ///
    /// Idx must be smaller the length of the vector.
    #[inline]
    pub unsafe fn get_unchecked(&self, idx: usize) -> T {
        (*self.0.get()).get_unchecked(idx).clone()
    }

    #[inline]
    pub fn resize(&mut self, new_len: usize, value: T) {
        unsafe { (*self.0.get()).resize(new_len, value) }
    }

    pub fn for_each<F: Fn(T)>(&self, f: F) {
        unsafe { self.unsafe_iter().cloned().for_each(f) }
    }
}
