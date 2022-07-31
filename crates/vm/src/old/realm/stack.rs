use std::{
    alloc::{self, Layout},
    mem,
    ptr::{self, NonNull},
    thread,
};

use crate::Value;
use dreck::{rebind, Bound, Gc, Root, Trace, Tracer};

pub type GcUpvalueObject<'gc, 'own> = Gc<'gc, 'own, UpvalueObject<'gc, 'own>>;

pub struct UpvalueObject<'gc, 'own> {
    location: *mut Value<'gc, 'own>,
    closed: Value<'gc, 'own>,
}

impl<'gc, 'own> UpvalueObject<'gc, 'own> {
    pub unsafe fn close(&mut self) {
        self.closed = self.location.read();
        self.location = &mut self.closed;
    }

    pub fn read(&self) -> Value<'gc, 'own> {
        unsafe { self.location.read() }
    }

    pub fn write(&mut self, v: Value<'_, 'own>) {
        unsafe { self.location.write(dreck::rebind(v)) }
    }
}

unsafe impl<'gc, 'own> Trace<'own> for UpvalueObject<'gc, 'own> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, trace: Tracer<'a, 'own>) {
        self.closed.trace(trace);
    }
}

unsafe impl<'a, 'gc, 'own> Bound<'a> for UpvalueObject<'gc, 'own> {
    type Rebound = UpvalueObject<'a, 'own>;
}

/// The vm stack implementation.
pub struct Stack<'gc, 'own> {
    /// Start of the stack array.
    root: NonNull<Value<'gc, 'own>>,
    /// points to the start of the current frame i.e. the first register in use.
    frame: *mut Value<'gc, 'own>,
    /// points one past the last value in use.
    stack: *mut Value<'gc, 'own>,

    capacity: usize,

    pub registers: u32,
    /// Amount of values allocated for the stack
    upvalues: Vec<GcUpvalueObject<'gc, 'own>>,
    frame_upvalues: u16,

    depth: u16,
}

impl<'gc, 'own> Drop for Stack<'gc, 'own> {
    fn drop(&mut self) {
        if self.capacity != 0 {
            let layout = Layout::array::<Value>(self.capacity).unwrap();
            unsafe {
                alloc::dealloc(self.root.as_ptr().cast(), layout);
            }
        }
    }
}

unsafe impl<'gc, 'own> Trace<'own> for Stack<'gc, 'own> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, trace: Tracer<'a, 'own>) {
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

unsafe impl<'a, 'gc, 'own> Bound<'a> for Stack<'gc, 'own> {
    type Rebound = Stack<'a, 'own>;
}

pub struct FrameGuard {
    size: u32,
    upvalues: u16,
    #[cfg(debug_assertions)]
    depth: u16,
}

#[cfg(debug_assertions)]
impl Drop for FrameGuard {
    fn drop(&mut self) {
        if !thread::panicking() {
            panic!("dropped frame guard instead of poping frame");
        }
    }
}

impl<'gc, 'own> Stack<'gc, 'own> {
    const MIN_CAPACITY: usize = 8;
    // Rust has a long standing but where match blocks stack usages grows linearly with the amount
    // of matches
    // This means that with the debug profile the depth the vm can goto is really limited.
    #[cfg(debug_assertions)]
    const MAX_DEPTH: u16 = 20;
    #[cfg(not(debug_assertions))]
    const MAX_DEPTH: u16 = 10_000;

    pub const DEPTH_EXCEEDED_MSG: &'static str = "exceeded max stack depth";

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

    pub unsafe fn push_frame(&mut self, size: u32) -> Option<FrameGuard> {
        if self.depth >= Self::MAX_DEPTH {
            return None;
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

        Some(res)
    }

    /// # Safety
    ///
    /// User must ensure that the guard is the same gaurd as the frame that was pushed at this
    /// depth
    pub unsafe fn pop_frame(&mut self, arena: &Root<'own>, guard: FrameGuard) {
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
    pub unsafe fn read_arg(&self, reg: u32) -> Option<Value<'gc, 'own>> {
        let ptr = self.frame.add(reg as usize);
        if ptr < self.stack {
            Some(ptr.read())
        } else {
            None
        }
    }

    #[inline(always)]
    pub unsafe fn read(&self, reg: u8) -> Value<'gc, 'own> {
        debug_assert!((reg as u32) < self.registers);
        self.frame.add(reg as usize).read()
    }

    #[inline(always)]
    pub unsafe fn write(&mut self, reg: u8, v: Value<'_, 'own>) {
        debug_assert!((reg as u32) < self.registers);
        self.frame.add(reg as usize).write(dreck::rebind(v))
    }

    pub unsafe fn push(&mut self, v: Value<'_, 'own>) {
        let new_size = self.stack.offset_from(self.root.as_ptr()) as usize + 1;
        if new_size > self.capacity {
            self.grow(new_size);
        }

        self.stack.write(dreck::rebind(v));
        self.stack = self.stack.add(1);
    }

    #[allow(unused_unsafe)]
    pub unsafe fn create_upvalue<'l>(
        &mut self,
        arena: &'l Root<'own>,
        reg: u8,
    ) -> GcUpvalueObject<'l, 'own> {
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
        self.upvalues.push(dreck::rebind(res));
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
