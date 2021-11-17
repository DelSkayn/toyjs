use std::{
    alloc::{self, GlobalAlloc, Layout, System},
    mem,
    ptr::{self, NonNull},
};

use crate::{gc::Trace, Value};

use super::reader::InstructionReader;

pub enum Frame {
    /// A frame which signifies an entry into the interperter.
    /// If this frame is popped the interperter should return from the execution loop
    Entry { registers: u8 },
    /// A call created from the interperter
    Call { registers: u8, data: CallFrameData },
    /// An exception guard, if an excreption is thrown execution should contin
    Try(TryFrameData),
}

pub struct CallFrameData {
    /// The destination to place the return value in.
    pub dst: u8,
    /// The data of the this frame.
    pub reader: InstructionReader,
}

pub struct TryFrameData {
    /// The destination to place to put the exception .
    pub dst: u8,
    /// The data of the this frame.
    pub jump: *mut u8,
}

/// The vm stack implementation.
pub struct Stack {
    root: NonNull<Value>,
    frame: *mut Value,
    stack: *mut Value,

    frames: Vec<Frame>,

    num_registers: u8,
    capacity: usize,
}

impl Stack {
    pub fn new() -> Self {
        let root = NonNull::dangling();
        Stack {
            frame: root.as_ptr(),
            stack: root.as_ptr(),
            root,
            frames: Vec::new(),
            num_registers: 0,
            capacity: 0,
        }
    }

    /// Enter a base stack frame.
    /// When the frame returns execution should be
    pub fn enter(&mut self, registers: u8) {
        unsafe {
            let new_used = self.used() + registers as usize;
            if new_used > self.capacity {
                self.grow(new_used)
            }
            self.frame = self.frame.add(self.num_registers as usize);
            self.frames.push(Frame::Entry {
                registers: self.num_registers,
            });
            let new_stack = self.frame.add(registers as usize);
            while self.stack < new_stack {
                self.stack.write(Value::undefined());
                self.stack = self.stack.add(1);
            }
            self.num_registers = registers;
        }
    }

    pub fn enter_call(&mut self, registers: u8, frame: CallFrameData) {
        unsafe {
            let new_used = self.used() + registers as usize;
            if new_used > self.capacity {
                self.grow(new_used)
            }
            self.frame = self.frame.add(self.num_registers as usize);
            let mut cur = self.stack;
            self.stack = self.frame.add(registers as usize);
            while cur < self.stack {
                cur.write(Value::undefined());
                cur = cur.add(1);
            }
            self.frames.push(Frame::Call {
                registers,
                data: frame,
            });
            self.num_registers = registers;
        }
    }

    pub fn guard(&mut self, frame: TryFrameData) {
        self.frames.push(Frame::Try(frame));
    }

    pub unsafe fn unguard(&mut self) {
        let frame = self.frames.pop();
        #[cfg(debug_assertions)]
        match frame {
            Some(Frame::Try(_)) => {}
            _ => panic!("tried to unguard stack which was not guarded"),
        }
    }

    pub fn unwind(&mut self) -> Option<TryFrameData> {
        loop {
            match self.frames.pop() {
                Some(Frame::Try(x)) => return Some(x),
                Some(Frame::Call { registers, .. }) => {
                    self.restore_frame(registers);
                }
                Some(Frame::Entry { registers, .. }) => {
                    self.restore_frame(registers);
                    return None;
                }
                None => panic!("root frame was not an entry frame"),
            }
        }
    }

    pub fn pop(&mut self) -> Option<CallFrameData> {
        loop {
            match self.frames.pop() {
                Some(Frame::Try(_)) => {}
                Some(Frame::Call { registers, data }) => {
                    self.restore_frame(registers);
                    return Some(data);
                }
                Some(Frame::Entry { registers, .. }) => {
                    self.restore_frame(registers);
                    return None;
                }
                None => panic!("tried to pop no existant stack"),
            }
        }
    }

    #[inline]
    pub fn push(&mut self, v: Value) {
        let used = self.used();
        if used + 1 < self.capacity {
            self.grow(used + 1)
        }
        unsafe {
            self.stack.write(v);
            self.stack = self.stack.add(1);
        }
    }

    #[inline]
    fn restore_frame(&mut self, registers: u8) {
        unsafe {
            self.stack = self.frame;
            self.frame = self.frame.sub(registers as usize);
            self.num_registers = registers;
        }
    }

    fn grow(&mut self, capacity: usize) {
        unsafe {
            let capacity = capacity.next_power_of_two();
            if self.capacity == 0 {
                let layout = Layout::array::<Value>(capacity).unwrap();
                let ptr = System.alloc(layout);
                if ptr == ptr::null_mut() {
                    alloc::handle_alloc_error(layout);
                }
                self.root = NonNull::new_unchecked(ptr.cast());
                self.frame = self.root.as_ptr();
                self.stack = self.root.as_ptr();
            } else {
                let layout = Layout::array::<Value>(self.capacity).unwrap();
                let size = capacity * mem::size_of::<Value>();
                let ptr = System
                    .realloc(self.root.as_ptr().cast(), layout, size)
                    .cast();
                if ptr == ptr::null_mut() {
                    alloc::handle_alloc_error(layout);
                }
                if ptr != self.root.as_ptr() {
                    self.stack = ptr.offset(self.stack.offset_from(self.root.as_ptr()));
                    self.frame = ptr.offset(self.frame.offset_from(self.root.as_ptr()));
                    self.root = NonNull::new_unchecked(ptr);
                }
            }
            self.capacity = capacity;
        }
    }

    #[inline]
    fn used(&self) -> usize {
        unsafe { self.stack.offset_from(self.root.as_ptr()) as usize }
    }

    #[inline(always)]
    pub fn read(&self, register: u8) -> Value {
        debug_assert!(register < self.num_registers);
        unsafe { self.frame.add(register as usize).read() }
    }

    #[inline(always)]
    pub fn write(&mut self, register: u8, v: Value) {
        debug_assert!(register < self.num_registers);
        unsafe { self.frame.add(register as usize).write(v) }
    }
}

unsafe impl Trace for Stack {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        for f in self.frames.iter() {
            match *f {
                Frame::Entry { .. } => {}
                Frame::Try(_) => {}
                Frame::Call { ref data, .. } => data.reader.trace(ctx),
            }
        }

        let mut cur = self.stack;
        while cur > self.root.as_ptr() {
            unsafe {
                cur = cur.sub(1);
                cur.read().trace(ctx);
            }
        }
    }
}
