use std::{
    alloc::{self, GlobalAlloc, Layout, System},
    fmt, mem,
    ptr::{self, NonNull},
};

use crate::{gc::Trace, Value};

/// The vm stack implementation.
pub struct Stack {
    root: NonNull<Value>,
    frame: *mut Value,
    stack: *mut Value,
    args: usize,
    capacity: usize,
}

unsafe impl Trace for Stack {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        unsafe {
            let mut cur = self.stack;
            while cur > self.root.as_ptr() {
                cur = cur.sub(1);
                cur.read().trace(ctx);
            }
        }
    }
}

impl Stack {
    pub const INITIAL_CAPACITY: usize = 256;

    /// Create a new stack with a capacity of [`Self::INITIAL_CAPACITY`].
    pub fn new() -> Self {
        unsafe {
            let layout = Layout::array::<Value>(Self::INITIAL_CAPACITY).unwrap();
            let root = alloc::alloc(layout) as *mut Value;
            Stack {
                stack: root,
                frame: root,
                root: NonNull::new(root).unwrap(),
                args: 0,
                capacity: Self::INITIAL_CAPACITY,
            }
        }
    }

    /// Reinitialize the current frame.
    /// Used in the case of a tail call.
    fn recall(&mut self, registers: u8) {
        unsafe {
            let used = self.frame.offset_from(self.root.as_ptr()) as usize;
            if used + registers as usize > self.capacity {
                self.grow();
            }
            let new = self.frame.add(registers as usize);
            while self.stack < new {
                self.stack.write(Value::empty());
                self.stack = self.stack.add(1);
            }
            self.stack = new;
        }
    }

    /// Allocates additional registers onto the stack.
    /// Should be called when a new function is about to be executed.
    pub fn enter_call(&mut self, registers: u8, args: u8) {
        unsafe {
            let used = self.stack.offset_from(self.root.as_ptr()) as usize;
            if used + registers as usize + 1 > self.capacity {
                self.grow();
            }
            let offset = self.stack.offset_from(self.frame);
            self.stack.write(Value::from(offset as i32));
            self.frame = self.stack.add(1);
            self.stack = self.stack.add(registers as usize + 1);
            let mut cur = self.frame.add(args as usize);
            while cur != self.stack {
                cur.write(Value::undefined());
                cur = cur.add(1);
            }
        }
    }

    /// Pops last allocated registers from the stack.
    /// Should be called when a function returns.
    pub unsafe fn exit_call(&mut self) {
        debug_assert!(self.stack != self.root.as_ptr());
        let registers = self.frame.sub(1).read().cast_int() as usize;
        self.stack = self.frame.sub(1);
        self.frame = self.stack.sub(registers);
    }

    //pub fn exit_call(&mut self,

    /// Read a value from the allocated registers
    ///
    /// # Safety
    ///
    /// Enough registers should be called for to access the given register.
    #[inline(always)]
    pub unsafe fn read(&self, register: u8) -> Value {
        self.frame.add(register as usize).read()
    }

    /// Write a value to one of the allocated registers
    ///
    /// # Safety
    ///
    /// Enough registers should be called for to access the given register.
    #[inline(always)]
    pub unsafe fn write(&mut self, register: u8, value: Value) {
        self.frame.add(register as usize).write(value)
    }

    /// Writes a new argument to the stack past the allocated registers.
    #[inline(always)]
    pub fn write_arg(&mut self, arg: u8, value: Value) {
        unsafe {
            let used = self.stack.offset_from(self.root.as_ptr()) as usize;
            if used + arg as usize + 1 > self.capacity {
                self.grow();
            }
            self.stack.add(1 + arg as usize).write(value)
        }
    }

    /// Reads a argument to the stack past the allocated registers.
    ///
    /// # Safety
    ///
    /// The arugment should have been allocated and written to.
    #[inline(always)]
    pub unsafe fn read_arg(&mut self, arg: u8) -> Value {
        let used = self.stack.offset_from(self.root.as_ptr()) as usize;
        if used + arg as usize + 1 > self.capacity {
            return Value::undefined();
        }
        self.stack.add(1 + arg as usize).read()
    }

    fn grow(&mut self) {
        unsafe {
            let new_capacity = (self.capacity + 1).next_power_of_two();
            let layout = Layout::array::<Value>(self.capacity).unwrap();
            let new_root = System.realloc(
                self.root.as_ptr() as *mut _,
                layout,
                new_capacity * mem::size_of::<Value>(),
            ) as *mut Value;

            if new_root.is_null() {
                panic!("could not allocated enough memory for stack");
            }

            if new_root != self.root.as_ptr() {
                self.frame = new_root.offset(self.frame.offset_from(self.root.as_ptr()));
                self.stack = new_root.offset(self.stack.offset_from(self.root.as_ptr()));
            }
            self.root = NonNull::new_unchecked(new_root);
            self.capacity = new_capacity;
        }
    }
}

impl Drop for Stack {
    fn drop(&mut self) {
        unsafe {
            if self.capacity > 0 {
                alloc::dealloc(
                    self.root.as_ptr() as *mut u8,
                    Layout::array::<Value>(self.capacity).unwrap(),
                )
            }
        }
    }
}

impl fmt::Debug for Stack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            let mut frames = Vec::new();

            let mut cur_frame = self.frame;
            while cur_frame.offset_from(self.root.as_ptr()) >= 1 {
                frames.push(cur_frame.sub(1));
                let val = cur_frame.sub(1).read();
                if !val.is_int() {
                    writeln!(f, "STACK CORRUPTED")?;
                    break;
                }
                cur_frame = cur_frame.sub(val.cast_int() as usize + 1);
            }

            let mut cur = self.root.as_ptr();
            writeln!(f, "STACK:")?;
            while cur != self.stack {
                if cur == frames.last().copied().unwrap_or(ptr::null_mut()) {
                    writeln!(f, "\t-- FRAME --")?;
                    frames.pop();
                }
                if cur == self.stack {
                    write!(f, "s>\t")?
                } else if cur == self.frame {
                    write!(f, "f>\t")?
                } else {
                    write!(f, "-\t")?
                }
                writeln!(f, "{:?}", cur.read())?;
                cur = cur.add(1);
            }
            Ok(())
        }
    }
}
