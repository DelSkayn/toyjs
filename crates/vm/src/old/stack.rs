use crate::{
    gc::{Ctx, Trace},
    JSValue,
};
use std::{
    alloc::{self, Layout},
    mem,
    ptr::{self, NonNull},
};

#[derive(Debug)]
pub struct Stack {
    data: NonNull<JSValue>,
    size: usize,
    //frame: *mut JSValue,
    stack: NonNull<JSValue>,
    frame: NonNull<JSValue>,
}

unsafe impl Trace for Stack {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: Ctx) {
        unsafe {
            let mut ptr = self.stack.as_ptr();
            while ptr != self.data.as_ptr() {
                ptr = ptr.sub(1);
                (*ptr).trace(ctx);
            }
        }
    }
}

impl Stack {
    pub fn new() -> Stack {
        Stack {
            size: 0,
            data: NonNull::dangling(),
            stack: NonNull::dangling(),
            frame: NonNull::dangling(),
        }
    }

    pub unsafe fn reserve_additional(&mut self, slots: usize) {
        if self.size == 0 {
            let layout = Layout::array::<JSValue>(slots.next_power_of_two()).unwrap();
            self.data = NonNull::new(alloc::alloc(layout) as *mut _).unwrap();
            self.stack = self.data;
            self.size = slots.next_power_of_two();
        } else {
            let used_size = self.stack.as_ptr().offset_from(self.data.as_ptr()) as usize + slots;
            if self.size < used_size {
                let new_size = used_size.next_power_of_two();
                let layout = Layout::array::<JSValue>(self.size).unwrap();
                let ptr = alloc::realloc(self.data.as_ptr() as *mut _, layout, new_size) as *mut _;
                self.size = new_size;
                if self.data.as_ptr() != ptr {
                    let stack_offset = self.stack.as_ptr().offset_from(self.data.as_ptr());
                    self.data = NonNull::new_unchecked(ptr as *mut _);
                    self.stack = NonNull::new_unchecked(self.data.as_ptr().offset(stack_offset));
                }
            }
        }
    }

    pub unsafe fn free_space(&mut self) {
        debug_assert!(self.data != NonNull::dangling());
        let used = self.stack.as_ptr().offset_from(self.data.as_ptr()) as usize;
        if used == 0 {
            let layout = Layout::array::<JSValue>(self.size).unwrap();
            alloc::dealloc(self.data.as_ptr() as *mut _, layout);
            self.size = 0;
        } else if used.next_power_of_two() < self.size {
            let layout = Layout::array::<JSValue>(self.size).unwrap();
            let ptr = alloc::realloc(
                self.data.as_ptr() as *mut _,
                layout,
                used.next_power_of_two(),
            ) as *mut _;
            if self.data.as_ptr() != ptr {
                let stack_offset = self.stack.as_ptr().offset_from(self.data.as_ptr());
                self.data = NonNull::new_unchecked(ptr as *mut _);
                self.stack = NonNull::new_unchecked(self.data.as_ptr().offset(stack_offset));
            }
        }
    }

    pub unsafe fn push_frame(&mut self, registers: u8) {
        // 1 additional to write the current frame size.
        self.reserve_additional(registers as usize);
        let mut ptr = self.stack.as_ptr();
        for _ in 0..registers {
            ptr.write(JSValue::undefined());
            ptr = ptr.add(1);
        }
        self.stack = NonNull::new_unchecked(ptr);
    }

    pub unsafe fn pop_frame(&mut self, registers: u8) {
        let mut ptr = self.stack.as_ptr();
        for _ in 0..registers {
            ptr = ptr.sub(1);
            ptr.drop_in_place();
        }
        debug_assert!(self.data.as_ptr() >= ptr);
        self.stack = NonNull::new_unchecked(ptr);
        self.free_space();
    }

    #[inline(always)]
    pub unsafe fn get(&self, register: u8) -> JSValue {
        (*self.stack.as_ptr().sub(register as usize + 1))
    }

    #[inline(always)]
    pub unsafe fn set(&mut self, register: u8, value: JSValue) {
        self.stack
            .as_ptr()
            .sub(register as usize + 1)
            .replace(value);
    }
}

impl Drop for Stack {
    fn drop(&mut self) {
        if self.stack.as_ptr() != self.data.as_ptr() {
            if !std::thread::panicking() {
                panic!("stack not freed")
            }
        }
        if self.size > 0 {
            let layout = Layout::array::<JSValue>(self.size).unwrap();
            unsafe {
                alloc::dealloc(self.data.as_ptr() as *mut u8, layout);
            }
        }
    }
}
