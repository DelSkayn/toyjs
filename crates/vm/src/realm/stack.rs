use std::{
    alloc::{self, Layout},
    cell::{Cell, UnsafeCell},
    convert::TryInto,
    isize,
    ptr::NonNull,
};

use crate::{gc::Trace, Gc, GcArena, Value};

use super::{ExecutionContext, InstructionReader};

#[derive(Debug)]
pub struct UpvalueObject {
    location: Cell<*mut Value>,
    closed: UnsafeCell<Value>,
}

unsafe impl Trace for UpvalueObject {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        unsafe {
            (*self.closed.get()).trace(ctx);
        }
    }
}

impl UpvalueObject {
    #[inline]
    pub unsafe fn close(&self) {
        (*self.closed.get()) = self.location.get().read();
        self.location.set(self.closed.get())
    }

    #[inline]
    pub unsafe fn write(&self, value: Value) {
        self.location.get().write(value);
    }

    #[inline]
    pub unsafe fn read(&self) -> Value {
        self.location.get().read()
    }
}

pub enum Frame {
    /// An entry frame from rust into the interperter.
    /// If this frame is popped the interperter should return to rust.
    Entry {
        // Size in number of registers of the frame.
        // A u32 because entry frames can contain a lot of values
        // that where pushed by rust code
        registers: u32,
        frame_offset: u8,
        open_upvalues: u16,
    },
    /// A frame of a javascript function call.
    Call {
        // Size in number of registers of the frame.
        registers: u8,
        /// Number of try frames between the current call frame and the previous.
        frame_offset: u8,
        open_upvalues: u16,
        data: CallFrameData,
    },
    Try {
        data: TryFrameData,
    },
}

pub struct CallFrameData {
    /// The register to put the return value into.
    pub dst: u8,
    /// The instruction reader
    pub instr: InstructionReader,
    pub ctx: ExecutionContext,
}

pub struct TryFrameData {
    /// The destination to place to put the exception .
    pub dst: u8,
    /// The data of the this frame.
    pub ip_offset: usize,
}

/// The vm stack implementation.
pub struct Stack {
    /// Start of the stack array.
    root: NonNull<Value>,
    /// points to the start of the current frame i.e. the first register in use.
    frame: *mut Value,
    /// points one past the last value in use.
    stack: *mut Value,

    frames: Vec<Frame>,

    cur_frame_size: u32,
    /// Amount of values allocated for the stack
    capacity: usize,
    /// Number of try frames between the current call frame and the previous.
    frame_offset: u8,

    open_upvalues: Vec<Gc<UpvalueObject>>,
    frame_open_upvalues: u16,
}

impl Stack {
    pub fn new() -> Self {
        let root = NonNull::dangling();
        Stack {
            frame: root.as_ptr(),
            stack: root.as_ptr(),
            root,
            frames: Vec::new(),
            cur_frame_size: 0,
            capacity: 0,
            frame_offset: 0,
            open_upvalues: Vec::new(),
            frame_open_upvalues: 0,
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
            self.frames.push(Frame::Entry {
                frame_offset: self.frame_offset,
                registers: self.cur_frame_size,
                open_upvalues: self.frame_open_upvalues,
            });
            self.frame_open_upvalues = 0;
            self.frame_offset = 0;
            self.frame = self.frame.add(self.cur_frame_size as usize);
            let new_stack = self.frame.add(registers as usize);
            while self.stack < new_stack {
                self.stack.write(Value::undefined());
                self.stack = self.stack.add(1);
            }
            self.cur_frame_size = registers as u32;
        }
    }

    pub fn enter_call(
        &mut self,
        new_registers: u8,
        dst: u8,
        instr: InstructionReader,
        ctx: ExecutionContext,
    ) {
        unsafe {
            let new_used = self.used() + new_registers as usize;
            if new_used > self.capacity {
                self.grow(new_used)
            }
            self.frame = self.frame.add(self.cur_frame_size as usize);
            let mut cur = self.stack;
            self.stack = self.frame.add(new_registers as usize);
            while cur < self.stack {
                cur.write(Value::undefined());
                cur = cur.add(1);
            }
            debug_assert!(
                self.cur_frame_size < u8::MAX as u32,
                "frame size to big to fit in u8"
            );
            self.frames.push(Frame::Call {
                registers: self.cur_frame_size as u8,
                data: CallFrameData { dst, instr, ctx },
                open_upvalues: self.frame_open_upvalues,
                frame_offset: self.frame_offset,
            });
            self.frame_offset = 0;
            self.cur_frame_size = new_registers as u32;
            self.frame_open_upvalues = 0;
        }
    }

    pub fn push_try(&mut self, dst: u8, ip_offset: usize) {
        self.frames.push(Frame::Try {
            data: TryFrameData { dst, ip_offset },
        });
        self.frame_offset += 1;
    }

    pub unsafe fn pop_try(&mut self) -> TryFrameData {
        let frame = self.frames.pop();
        match frame {
            Some(Frame::Try { data }) => data,
            _ => panic!("tried to unguard stack which was not guarded"),
        }
    }

    pub unsafe fn unwind(&mut self, gc: &GcArena) -> Option<Result<TryFrameData, CallFrameData>> {
        match self.frames.pop() {
            Some(Frame::Try { data }) => Some(Ok(data)),
            Some(Frame::Call {
                registers,
                frame_offset,
                open_upvalues,
                data,
            }) => {
                self.restore_frame(registers.into(), frame_offset, open_upvalues, gc);
                Some(Err(data))
            }
            Some(Frame::Entry {
                registers,
                frame_offset,
                open_upvalues,
            }) => {
                self.restore_frame(registers, frame_offset, open_upvalues, gc);
                None
            }
            None => panic!("root frame was not an entry frame"),
        }
    }

    pub unsafe fn pop(&mut self, gc: &GcArena) -> Option<CallFrameData> {
        loop {
            match self.frames.pop() {
                Some(Frame::Try { .. }) => {}
                Some(Frame::Call {
                    registers,
                    data,
                    frame_offset,
                    open_upvalues,
                }) => {
                    self.restore_frame(registers.into(), frame_offset, open_upvalues, gc);
                    return Some(data);
                }
                Some(Frame::Entry {
                    registers,
                    frame_offset,
                    open_upvalues,
                }) => {
                    self.restore_frame(registers, frame_offset, open_upvalues, gc);
                    self.frame_offset = 0;
                    return None;
                }
                None => panic!("tried to pop non-existant stack"),
            }
        }
    }

    pub unsafe fn create_upvalue(&mut self, register: u8, gc: &GcArena) -> Gc<UpvalueObject> {
        let location = self.frame.add(register as usize);

        let upvalue_frame = self.open_upvalues.len() - self.frame_open_upvalues as usize;
        for &u in self.open_upvalues[upvalue_frame..].iter() {
            if std::ptr::eq(u.location.get(), location) {
                return u;
            }
        }

        let upvalue = gc.allocate(UpvalueObject {
            location: Cell::new(location),
            closed: UnsafeCell::new(Value::empty()),
        });
        self.open_upvalues.push(upvalue);
        self.frame_open_upvalues += 1;
        upvalue
    }

    #[inline]
    pub fn push(&mut self, v: Value) {
        let used = self.used();
        if used + 1 > self.capacity {
            self.grow(used + 1)
        }
        unsafe {
            self.stack.write(v);
            self.stack = self.stack.add(1);
        }
    }

    pub fn push_temp(&mut self, v: Value) {
        let used = self.used();
        if used + 1 > self.capacity {
            self.grow(used + 1)
        }
        unsafe {
            self.stack.write(v);
            self.stack = self.stack.add(1);
            self.cur_frame_size = self
                .stack
                .offset_from(self.frame)
                .try_into()
                .expect("frame size exceeded u32::MAX")
        }
    }

    #[inline]
    fn restore_frame(
        &mut self,
        registers: u32,
        frame_offset: u8,
        open_upvalues: u16,
        gc: &GcArena,
    ) {
        unsafe {
            self.stack = self.frame;
            self.frame = self.frame.sub(registers as usize);
            self.cur_frame_size = registers;
            self.frame_offset = frame_offset;
            let upvalue_frame = self.open_upvalues.len() - self.frame_open_upvalues as usize;
            self.open_upvalues.drain(upvalue_frame..).for_each(|x| {
                gc.write_barrier(x);
                x.close()
            });
            self.frame_open_upvalues = open_upvalues;
        }
    }

    #[inline]
    unsafe fn rebase<T>(old: *mut T, new: *mut T, ptr: *mut T) -> *mut T {
        let offset = (ptr as isize) - (old as isize);
        new.cast::<u8>().offset(offset).cast()
    }

    fn grow(&mut self, capacity: usize) {
        unsafe {
            let capacity = capacity.next_power_of_two().max(8);
            if self.capacity == 0 {
                let layout = Layout::array::<Value>(capacity).unwrap();
                let ptr = alloc::alloc(layout);
                if ptr.is_null() {
                    alloc::handle_alloc_error(layout);
                }
                self.root = NonNull::new_unchecked(ptr.cast());
                self.frame = self.root.as_ptr();
                self.stack = self.root.as_ptr();
            } else {
                let layout = Layout::array::<Value>(self.capacity).unwrap();
                let size = Layout::array::<Value>(capacity).unwrap().size();
                let ptr: *mut Value =
                    alloc::realloc(self.root.as_ptr().cast(), layout, size).cast();
                if ptr.is_null() {
                    alloc::handle_alloc_error(layout);
                }
                if ptr != self.root.as_ptr() {
                    let original = self.root.as_ptr();
                    self.stack = Self::rebase(original, ptr, self.stack);
                    self.frame = Self::rebase(original, ptr, self.frame);
                    self.open_upvalues.iter_mut().for_each(|x| {
                        let new_loc = Self::rebase(original, ptr, x.location.get());
                        x.location.set(new_loc);
                    });
                    self.root = NonNull::new_unchecked(ptr);
                }
            }
            self.capacity = capacity;
        }
    }

    #[inline]
    pub fn frame_size(&self) -> usize {
        unsafe { self.stack.offset_from(self.frame) as usize }
    }

    #[inline]
    fn used(&self) -> usize {
        unsafe { self.stack.offset_from(self.root.as_ptr()) as usize }
    }

    #[inline(always)]
    pub fn read(&self, register: u8) -> Value {
        debug_assert!((register as usize) < self.frame_size());
        unsafe { self.frame.add(register as usize).read() }
    }

    #[inline(always)]
    pub fn write(&mut self, register: u8, v: Value) {
        debug_assert!((register as usize) < self.frame_size());
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
        #[cfg(feature = "dump-gc-trace")]
        println!("TRACE: stack.frames");

        for f in self.frames.iter() {
            match *f {
                Frame::Entry { .. } => {}
                Frame::Try { .. } => {}
                Frame::Call { ref data, .. } => {
                    data.ctx.trace(ctx);
                }
            }
        }

        #[cfg(feature = "dump-gc-trace")]
        println!("TRACE: stack.stack");
        let mut cur = self.stack;
        while cur > self.root.as_ptr() {
            unsafe {
                cur = cur.sub(1);
                cur.read().trace(ctx);
            }
        }
    }
}

impl Drop for Stack {
    fn drop(&mut self) {
        unsafe {
            alloc::dealloc(
                self.root.as_ptr().cast(),
                Layout::array::<Value>(self.capacity).unwrap(),
            )
        }
    }
}
