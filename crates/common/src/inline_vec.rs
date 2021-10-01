use std::{
    mem::MaybeUninit,
    ops::{Deref, DerefMut},
};

pub struct InlineVec<T, const LEN: usize> {
    array: [MaybeUninit<T>; LEN],
    len: usize,
}

impl<T, const LEN: usize> InlineVec<T, LEN> {
    /// Create a new empty inline vector.
    pub fn new() -> Self {
        let array = unsafe { MaybeUninit::<[MaybeUninit<T>; LEN]>::uninit().assume_init() };

        InlineVec { array, len: 0 }
    }

    pub fn push(&mut self, v: T) {
        self.array[self.len] = MaybeUninit::new(v);
        self.len += 1;
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.is_empty() {
            return None;
        }
        self.len -= 1;
        Some(unsafe { self.array[self.len].as_ptr().read() })
    }
}

impl<T, const LEN: usize> Deref for InlineVec<T, LEN> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        unsafe { &*(&self.array[0..self.len] as *const [MaybeUninit<T>] as *const [T]) }
    }
}

impl<T, const LEN: usize> DerefMut for InlineVec<T, LEN> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(&mut self.array[0..self.len] as *mut [MaybeUninit<T>] as *mut [T]) }
    }
}
