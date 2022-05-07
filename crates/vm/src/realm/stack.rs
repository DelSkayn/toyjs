use std::{
    alloc::{self, Layout},
    mem,
    ptr::{self, NonNull},
    thread,
};

use crate::{
    gc::{self, Arena, Gc, Rebind, Trace, Tracer},
    rebind, Value,
};

pub type GcUpvalueObject<'gc, 'cell> = Gc<'gc, 'cell, UpvalueObject<'gc, 'cell>>;

pub struct UpvalueObject<'gc, 'cell> {
    location: *mut Value<'gc, 'cell>,
    closed: Value<'gc, 'cell>,
}

impl<'gc, 'cell> UpvalueObject<'gc, 'cell> {
    pub unsafe fn close(&mut self) {
        self.closed = self.location.read();
        self.location = &mut self.closed;
    }

    pub fn read(&self) -> Value<'gc, 'cell> {
        unsafe { self.location.read() }
    }

    pub fn write(&mut self, v: Value<'_, 'cell>) {
        unsafe { self.location.write(gc::rebind(v)) }
    }
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

unsafe impl<'a, 'gc, 'cell> Rebind<'a> for UpvalueObject<'gc, 'cell> {
    type Output = UpvalueObject<'a, 'cell>;
}

/// The vm stack implementation.
pub struct Stack<'gc, 'cell> {
    /// Start of the stack array.
    root: NonNull<Value<'gc, 'cell>>,
    /// points to the start of the current frame i.e. the first register in use.
    frame: *mut Value<'gc, 'cell>,
    /// points one past the last value in use.
    stack: *mut Value<'gc, 'cell>,

    capacity: usize,

    pub registers: u32,
    /// Amount of values allocated for the stack
    upvalues: Vec<GcUpvalueObject<'gc, 'cell>>,
    frame_upvalues: u16,

    depth: u32,
}

impl<'gc, 'cell> Drop for Stack<'gc, 'cell> {
    fn drop(&mut self) {
        if self.capacity != 0 {
            let layout = Layout::array::<Value>(self.capacity).unwrap();
            unsafe {
                alloc::dealloc(self.root.as_ptr().cast(), layout);
            }
        }
    }
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
            let mut cur = self.root.as_ptr();
            while cur < self.stack {
                cur.read().trace(trace);
                cur = cur.add(1);
            }
        }
    }
}

pub struct FrameGuard {
    size: u32,
    upvalues: u16,
    #[cfg(debug_assertions)]
    depth: u32,
}

#[cfg(debug_assertions)]
impl Drop for FrameGuard {
    fn drop(&mut self) {
        if !thread::panicking() {
            panic!("dropped frame guard instead of poping frame");
        }
    }
}

impl<'gc, 'cell> Stack<'gc, 'cell> {
    const MIN_CAPACITY: usize = 8;
    const MAX_DEPTH: u32 = 1 << 20;

    pub fn new() -> Self {
        let root = NonNull::dangling();
        Stack {
            frame: root.as_ptr(),
            stack: root.as_ptr(),
            root,
            capacity: 0,

            registers: 0,

            upvalues: Vec::new(),
            frame_upvalues: 0,

            depth: 0,
        }
    }

    pub unsafe fn push_frame(&mut self, size: u32) -> FrameGuard {
        if self.depth >= Self::MAX_DEPTH {
            panic!("stack exceeded max depth");
        }

        let res = FrameGuard {
            size: self.registers,
            upvalues: self.frame_upvalues,
            #[cfg(debug_assertions)]
            depth: self.depth,
        };
        self.frame = self.frame.add(self.registers as usize);

        let new_size = self.frame.offset_from(self.root.as_ptr()) as usize + size as usize + 1;
        if new_size > self.capacity {
            self.grow(new_size);
        }

        let new_stack = self.frame.add(size as usize);
        while self.stack < new_stack {
            self.stack.write(Value::undefined());
            self.stack = self.stack.add(1);
        }

        self.frame_upvalues = 0;
        self.registers = size;
        self.depth += 1;

        res
    }

    /// # Safety
    ///
    /// User must ensure that the guard is the same gaurd as the frame that was pushed at this
    /// depth
    pub unsafe fn pop_frame(&mut self, arena: &Arena<'_, 'cell>, guard: FrameGuard) {
        self.depth -= 1;

        #[cfg(debug_assertions)]
        assert_eq!(guard.depth, self.depth);

        self.stack = self.frame;
        self.frame = self.frame.sub(guard.size as usize);

        let old_len = self.upvalues.len() - self.frame_upvalues as usize;
        self.upvalues.drain(old_len..).for_each(|x| {
            arena.write_barrier(x);
            (*x.get()).close()
        });

        self.frame_upvalues = guard.upvalues;
        self.registers = guard.size;

        mem::forget(guard);
    }

    #[inline(always)]
    pub unsafe fn frame_size(&self) -> usize {
        self.stack.offset_from(self.frame) as usize
    }

    #[inline(always)]
    pub unsafe fn read_arg(&self, reg: u32) -> Option<Value<'gc, 'cell>> {
        let ptr = self.frame.add(reg as usize);
        if ptr < self.stack {
            Some(ptr.read())
        } else {
            None
        }
    }

    #[inline(always)]
    pub unsafe fn read(&self, reg: u8) -> Value<'gc, 'cell> {
        debug_assert!((reg as u32) < self.registers);
        self.frame.add(reg as usize).read()
    }

    #[inline(always)]
    pub unsafe fn write(&mut self, reg: u8, v: Value<'_, 'cell>) {
        debug_assert!((reg as u32) < self.registers);
        self.frame.add(reg as usize).write(gc::rebind(v))
    }

    pub unsafe fn push(&mut self, v: Value<'_, 'cell>) {
        let new_size = self.stack.offset_from(self.root.as_ptr()) as usize + 1;
        if new_size > self.capacity {
            self.grow(new_size);
        }

        self.stack.write(gc::rebind(v));
        self.stack = self.stack.add(1);
    }

    #[allow(unused_unsafe)]
    pub unsafe fn create_upvalue<'l>(
        &mut self,
        arena: &'l Arena<'_, 'cell>,
        reg: u8,
    ) -> GcUpvalueObject<'l, 'cell> {
        let location = self.frame.add(reg as usize);

        let start = self.upvalues.len() - self.frame_upvalues as usize;
        for u in &self.upvalues[start..] {
            if ptr::eq((*u.get()).location, location) {
                return rebind!(arena, *u);
            }
        }

        let res = UpvalueObject {
            location,
            closed: Value::empty(),
        };
        let res = arena.add(res);
        self.upvalues.push(gc::rebind(res));
        self.frame_upvalues += 1;
        res
    }

    #[inline]
    unsafe fn rebase<T>(old: *mut T, new: *mut T, ptr: *mut T) -> *mut T {
        let offset = (ptr as isize) - (old as isize);
        new.cast::<u8>().offset(offset).cast()
    }

    #[cold]
    unsafe fn grow(&mut self, capacity: usize) {
        if self.capacity == 0 {
            self.capacity = capacity.max(Self::MIN_CAPACITY);
            let layout = Layout::array::<Value>(self.capacity).unwrap();
            let ptr = alloc::alloc(layout);
            if ptr.is_null() {
                alloc::handle_alloc_error(layout);
            }
            self.root = NonNull::new_unchecked(ptr.cast());
            self.frame = self.root.as_ptr();
            self.stack = self.root.as_ptr();
        } else {
            let old_capacity = self.capacity;
            self.capacity = capacity.next_power_of_two();
            let layout = Layout::array::<Value>(old_capacity).unwrap();
            let size = Layout::array::<Value>(self.capacity).unwrap().size();
            let ptr: *mut Value = alloc::realloc(self.root.as_ptr().cast(), layout, size).cast();
            if ptr.is_null() {
                alloc::handle_alloc_error(layout);
            }
            if ptr != self.root.as_ptr() {
                let original = self.root.as_ptr();
                self.stack = Self::rebase(original, ptr, self.stack);
                self.frame = Self::rebase(original, ptr, self.frame);
                // Stack has unique accesso
                for up in &self.upvalues {
                    let up_ptr: *mut UpvalueObject = up.get();
                    let loc = (*up_ptr).location;
                    let loc = Self::rebase(original, ptr, loc);
                    // Safe no new pointer is added to the upvalue
                    (*up_ptr).location = loc;
                }
                self.root = NonNull::new_unchecked(ptr);
            }
        }
    }
}
