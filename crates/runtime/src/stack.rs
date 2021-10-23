use std::{
    alloc::{self, Layout},
    fmt,
    ptr::NonNull,
};

use crate::{gc::Trace, JSValue};

pub struct Stack {
    root: NonNull<JSValue>,
    stack: *mut JSValue,
    frame: *mut JSValue,
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
            let mut cur = self.frame;
            while cur > self.root.as_ptr() {
                cur = cur.sub(1);
                cur.read().trace(ctx);
            }
        }
    }
}

impl Stack {
    const INITIAL_CAPACITY: usize = 256;

    pub fn new() -> Self {
        unsafe {
            let layout = Layout::array::<JSValue>(Self::INITIAL_CAPACITY).unwrap();
            let root = alloc::alloc(layout) as *mut JSValue;
            Stack {
                frame: root,
                stack: root,
                root: NonNull::new(root).unwrap(),
                args: 0,
                capacity: Self::INITIAL_CAPACITY,
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
            let mut cur = self.stack.add(self.args);
            while cur != self.frame {
                cur.write(JSValue::undefined());
                cur = cur.add(1);
            }
            self.args = 0;
        }
    }

    pub unsafe fn exit_call(&mut self) {
        debug_assert!(self.frame != self.root.as_ptr());
        let registers = self.stack.sub(1).read().into_int() as usize;
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

    #[inline(always)]
    pub unsafe fn write_arg(&mut self, arg: u8, value: JSValue) {
        let used = self.frame.offset_from(self.root.as_ptr()) as usize;
        if used + arg as usize + 1 > self.capacity {
            self.grow();
        }
        self.args = self.args.max(arg as usize + 1);
        self.frame.add(1 + arg as usize).write(value)
    }

    #[inline(always)]
    pub unsafe fn read_arg(&mut self, arg: u8) -> JSValue {
        let used = self.frame.offset_from(self.root.as_ptr()) as usize;
        if used + arg as usize + 1 > self.capacity {
            return JSValue::undefined();
        }
        self.frame.add(1 + arg as usize).read()
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

impl fmt::Debug for Stack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            let mut cur = self.root.as_ptr();
            writeln!(f, "STACK:")?;
            while cur != self.frame {
                if cur == self.frame {
                    write!(f, "f>\t")?
                } else if cur == self.stack {
                    write!(f, "s>\t")?
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
