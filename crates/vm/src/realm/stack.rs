use std::{
    alloc::{self, Layout},
    cell::{Cell, UnsafeCell},
    ptr::{self, NonNull},
};

use crate::{
    cell::CellOwner,
    gc::{self, Gc, Trace, Tracer},
    Value,
};

type GcUpvalueObject<'gc, 'cell> = Gc<'gc, 'cell, UpvalueObject<'gc, 'cell>>;

pub struct UpvalueObject<'gc, 'cell> {
    location: *mut Value<'gc, 'cell>,
    closed: Value<'gc, 'cell>,
}

unsafe impl<'gc, 'cell> Trace for UpvalueObject<'gc, 'cell> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, trace: Tracer) {
        self.closed.trace(trace);
    }
}

/// The vm stack implementation.
pub struct Stack<'gc, 'cell> {
    /// Start of the stack array.
    root: Cell<NonNull<Value<'gc, 'cell>>>,
    /// points to the start of the current frame i.e. the first register in use.
    frame: Cell<*mut Value<'gc, 'cell>>,
    /// points one past the last value in use.
    stack: Cell<*mut Value<'gc, 'cell>>,

    cur_frame_size: Cell<u32>,
    /// Amount of values allocated for the stack
    capacity: Cell<usize>,
    open_upvalues: UnsafeCell<Vec<GcUpvalueObject<'gc, 'cell>>>,
    frame_open_upvalues: Cell<u16>,
}

unsafe impl<'gc, 'cell> Trace for Stack<'gc, 'cell> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, trace: Tracer) {
        // Upvalues dont need to be traced since all of them are open and contain no pointer
        // values.
        unsafe {
            let mut cur = self.root.get().as_ptr();
            while cur != self.stack.get() {
                cur.read().trace(trace);
                cur = cur.add(1);
            }
        }
    }
}

impl<'gc, 'cell> Stack<'gc, 'cell> {
    pub fn new() -> Self {
        Stack {
            root: Cell::new(NonNull::dangling()),
            frame: Cell::new(ptr::null_mut()),
            stack: Cell::new(ptr::null_mut()),

            cur_frame_size: Cell::new(0),

            capacity: Cell::new(0),
            open_upvalues: UnsafeCell::new(Vec::new()),
            frame_open_upvalues: Cell::new(0),
        }
    }

    pub unsafe fn read(&self, reg: u8) -> Value<'gc, 'cell> {
        debug_assert!((reg as u32) < self.cur_frame_size.get());
        self.frame.get().add(reg as usize).read()
    }

    pub unsafe fn write<'a>(&self, reg: u8, v: Value<'a, 'cell>) {
        debug_assert!((reg as u32) < self.cur_frame_size.get());
        self.frame.get().add(reg as usize).write(gc::_rebind(v))
    }

    #[inline]
    unsafe fn rebase<T>(old: *mut T, new: *mut T, ptr: *mut T) -> *mut T {
        let offset = (ptr as isize) - (old as isize);
        new.cast::<u8>().offset(offset).cast()
    }

    #[cold]
    fn grow(&self, capacity: usize, owner: &mut CellOwner<'cell>) {
        unsafe {
            let capacity = capacity.next_power_of_two().max(8);
            if self.capacity.get() == 0 {
                let layout = Layout::array::<Value>(capacity).unwrap();
                let ptr = alloc::alloc(layout);
                if ptr.is_null() {
                    alloc::handle_alloc_error(layout);
                }
                self.root.set(NonNull::new_unchecked(ptr.cast()));
                self.frame.set(self.root.get().as_ptr());
                self.stack.set(self.root.get().as_ptr());
            } else {
                let layout = Layout::array::<Value>(self.capacity.get()).unwrap();
                let size = Layout::array::<Value>(capacity).unwrap().size();
                let ptr: *mut Value =
                    alloc::realloc(self.root.get().as_ptr().cast(), layout, size).cast();
                if ptr.is_null() {
                    alloc::handle_alloc_error(layout);
                }
                if ptr != self.root.get().as_ptr() {
                    let original = self.root.get().as_ptr();
                    self.stack
                        .set(Self::rebase(original, ptr, self.stack.get()));
                    self.frame
                        .set(Self::rebase(original, ptr, self.frame.get()));
                    for up in &(*self.open_upvalues.get()) {
                        let loc = up.borrow(owner).location;
                        let loc = Self::rebase(original, ptr, loc);
                        up.borrow_mut_untraced_unchecked(owner).location = loc;
                    }
                    self.root.set(NonNull::new_unchecked(ptr));
                }
            }
            self.capacity.set(capacity);
        }
    }
}
