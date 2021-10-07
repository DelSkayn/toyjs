use std::{
    alloc::{self, Layout},
    ptr::NonNull,
};

use crate::{gc::Trace, JSValue};

pub struct Stack {
    root: NonNull<JSValue>,
    stack: *mut JSValue,
    frame: *mut JSValue,
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
            let mut cur = self.frame;
            while cur > self.root.as_ptr() {
                cur = cur.sub(1);
                self.frame.read().trace(ctx);
            }
        }
    }
}

impl Stack {
    pub fn new() -> Self {
        unsafe {
            let layout = Layout::array::<JSValue>(255).unwrap();
            let root = alloc::alloc(layout) as *mut JSValue;
            Stack {
                frame: root,
                stack: root,
                root: NonNull::new(root).unwrap(),
                capacity: 256,
            }
        }
    }

    /// Reinitialize the current frame.
    /// Used in the case of a tail call.
    pub fn recall(&mut self, registers: u8) {
        unsafe {
            let used = self.stack.offset_from(self.root.as_ptr()) as usize;
            if used + registers as usize > self.capacity {
                self.grow();
            }
            let new = self.stack.add(registers as usize);
            while self.frame < new {
                self.frame.write(JSValue::empty());
                self.frame = self.frame.add(1);
            }
            self.frame = new;
        }
    }

    pub fn enter_call(&mut self, registers: u8) {
        unsafe {
            let used = self.frame.offset_from(self.root.as_ptr()) as usize;
            if used + registers as usize + 1 > self.capacity {
                self.grow();
            }
            self.frame.write(JSValue::from(registers as i32));
            self.stack = self.frame.add(1);
            self.frame = self.frame.add(registers as usize + 1);
            let mut cur = self.stack;
            while cur != self.frame {
                cur.write(JSValue::empty());
                cur = cur.add(1);
            }
        }
    }

    pub unsafe fn exit_call(&mut self) {
        debug_assert!(self.frame != self.root.as_ptr());
        let registers = self.frame.sub(1).read().into_int() as usize;
        self.frame = self.stack.sub(1);
        self.stack = self.frame.sub(registers);
    }

    //pub fn exit_call(&mut self,

    #[inline(always)]
    pub unsafe fn read(&self, register: u8) -> JSValue {
        self.stack.add(register as usize).read()
    }

    #[inline(always)]
    pub unsafe fn write(&mut self, register: u8, value: JSValue) {
        self.stack.add(register as usize).write(value)
    }

    fn grow(&mut self) {
        todo!()
    }
}

impl Drop for Stack {
    fn drop(&mut self) {
        unsafe {
            if self.capacity > 0 {
                alloc::dealloc(
                    self.root.as_ptr() as *mut u8,
                    Layout::array::<JSValue>(self.capacity).unwrap(),
                )
            }
        }
    }
}
