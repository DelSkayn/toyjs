use std::{
    alloc::Layout,
    ptr::{self, NonNull},
};

/// A growable raw buffer with support for relocating raw pointers on alloc.
pub struct RawBuffer<T> {
    root: NonNull<T>,
    end: NonNull<T>,
}

impl<T> RawBuffer<T> {
    pub const MIN_ALLOC_SIZE: usize = 8;

    pub fn new() -> Self {
        RawBuffer {
            root: NonNull::dangling(),
            end: NonNull::dangling(),
        }
    }

    pub fn root(&self) -> NonNull<T> {
        self.root
    }

    pub fn end(&self) -> NonNull<T> {
        self.end
    }

    pub fn capacity(&self) -> usize {
        unsafe { self.end.as_ptr().offset_from(self.root.as_ptr()) as usize }
    }

    /// Change the size the stack to the new size.
    pub fn grow<F>(&mut self, new_size: usize, update: F)
    where
        F: FnOnce(NonNull<T>, NonNull<T>),
    {
        if ptr::eq(self.root.as_ptr(), self.end.as_ptr()) {
            unsafe {
                let layout = Layout::array::<T>(new_size).expect("stack too big");
                let mem = std::alloc::alloc(layout).cast::<T>();
                let root = NonNull::new(mem).expect("allocation failed");
                update(self.root, root);
                self.root = root;
                self.end = NonNull::new_unchecked(root.as_ptr().add(new_size));
            }
        } else {
            let capacity = self.capacity();
            assert!(new_size >= capacity);
            let old_layout = Layout::array::<T>(capacity).unwrap();
            let new_mem =
                unsafe { std::alloc::realloc(self.root.as_ptr().cast(), old_layout, new_size) }
                    .cast::<T>();

            let new_mem = NonNull::new(new_mem).expect("failed to allocate memory for stack");
            self.end = unsafe { NonNull::new_unchecked(new_mem.as_ptr().add(new_size)) };

            if ptr::eq(new_mem.as_ptr(), self.root.as_ptr()) {
                // Pointer was reallocated in place, no need to update pointers.
                return;
            }

            update(self.root, new_mem);
            self.root = new_mem;
        }
    }
}

impl<T> Drop for RawBuffer<T> {
    fn drop(&mut self) {
        // RawBuffer does not take care of dropping values,
        // Only deallocation of memory
        if self.root() == self.end() {
            return;
        }
        let capacity = self.capacity();
        let layout = Layout::array::<T>(capacity).unwrap();
        unsafe {
            std::alloc::dealloc(self.root().as_ptr().cast(), layout);
        }
    }
}
