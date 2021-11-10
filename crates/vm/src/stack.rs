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
    num_registers: u8,
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
            let mut cur = self.stack.sub(2);
            // Dont need to trace the first value as it will always be a integer.
            while cur > self.root.as_ptr() {
                cur.read().trace(ctx);
                cur = cur.sub(1);
            }
        }
    }
}

impl Stack {
    pub const INITIAL_CAPACITY: usize = 8;

    /// Create a new stack with a capacity of [`Self::INITIAL_CAPACITY`].
    pub fn new() -> Self {
        unsafe {
            let layout = Layout::array::<Value>(Self::INITIAL_CAPACITY).unwrap();
            let root = alloc::alloc(layout) as *mut Value;
            Stack {
                stack: root,
                frame: root,
                root: NonNull::new(root).unwrap(),
                num_registers: 0,
                capacity: Self::INITIAL_CAPACITY,
            }
        }
    }

    // Reinitialize the current frame.
    // Used in the case of a tail call.
    /*
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
    */

    /// Allocates additional registers onto the stack.
    /// Should be called when a new function is about to be executed.
    pub fn enter_call(&mut self, registers: u8) {
        unsafe {
            let used = self.frame.offset_from(self.root.as_ptr()) as usize;
            // The already used values, the current registers in use, the new registers and one for
            // to store the value to recover the frame.
            let new_capacity = used + self.num_registers as usize + registers as usize + 1;
            if new_capacity > self.capacity {
                self.grow(new_capacity);
            }
            // Write the amount of current registers to just after the currently live registers.
            // New frame ptr points to the first new register.
            self.frame = self.frame.add(self.num_registers as usize);
            self.frame.write(Value::from(self.num_registers as i32));
            self.frame = self.frame.add(1);

            self.num_registers = registers;

            // From the current stack ptr to the where the new registers are stored are
            // uninitialized so they need to be initialized.
            let new_stack = self.frame.add(registers as usize);
            while self.stack != new_stack {
                self.stack.write(Value::undefined());
                self.stack = self.stack.add(1);
            }
            // The stack pointer points to 2(!) values after the last new register.
            // This is where possible arguments would be writen to.
            //
            // --+----------+-----------+-----------+
            // ..| last reg | invalid 1 | invalid 2 |
            // --+----------+-----------+-----------+
            //                          ^
            //                       stack ptr
            self.stack = self.stack.add(1);
        }
    }

    /// Pops last allocated registers from the stack.
    /// Should be called when a function returns.
    pub unsafe fn exit_call(&mut self) {
        debug_assert!(self.stack != self.root.as_ptr());
        self.stack = self.frame;
        self.num_registers = self.frame.sub(1).read().cast_int() as u8;
        self.frame = self.stack.sub(self.num_registers as usize + 1);
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

    /// Writes a new value to the stack past the current frame.
    #[inline(always)]
    pub fn push(&mut self, value: Value) {
        unsafe {
            let used = self.stack.offset_from(self.root.as_ptr()) as usize;
            if used + 1 > self.capacity {
                self.grow(used + 1);
            }
            self.stack.write(value);
            self.stack = self.stack.add(1);
        }
    }

    fn grow(&mut self, new_capacity: usize) {
        unsafe {
            let new_capacity = new_capacity.next_power_of_two();
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
