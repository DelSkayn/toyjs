use std::{
    alloc::{Allocator, Global},
    cell::UnsafeCell,
};

/// A vector which allows push and poping from a immutable refrence but does not allow
/// obtaining references to internal values.
pub struct CellVec<T, A: Allocator = Global>(UnsafeCell<Vec<T, A>>);

/// It is save to send the vector across threads since there can be no references to internal
/// values.
unsafe impl<T: Send, A: Allocator + Send> Send for CellVec<T, A> {}

impl<T> CellVec<T, Global> {
    pub fn new() -> Self {
        Self::new_in(Global)
    }
}

impl<T, A: Allocator> CellVec<T, A> {
    pub fn new_in(alloc: A) -> Self {
        CellVec(UnsafeCell::new(Vec::new_in(alloc)))
    }

    pub fn push(&self, value: T) {
        unsafe { (*self.0.get()).push(value) }
    }

    pub fn pop(&self) -> Option<T> {
        unsafe { (*self.0.get()).pop() }
    }

    pub fn len(&self) -> usize {
        unsafe { (*self.0.get()).len() }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn clear(&self) {
        unsafe { (*self.0.get()).clear() }
    }
}

impl<T: Clone, A: Allocator> CellVec<T, A> {
    pub fn get(&self, idx: usize) -> Option<T> {
        unsafe { (*self.0.get()).get(idx).cloned() }
    }
}
