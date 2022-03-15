use std::{
    alloc::{self, GlobalAlloc, Layout, System},
    cell::{Cell, UnsafeCell},
    convert::TryInto,
    marker::PhantomData,
    mem,
    ptr::NonNull,
};

use crate::{function::Function, gc::Trace, Gc, GcArena, Value};

use super::{reader::InstructionReader, Arguments};

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

    pub unsafe fn rebase(&self, original: *mut Value, new: *mut Value) {
        debug_assert!((*self.closed.get()) == Value::empty());
        self.location
            .set(new.offset(self.location.get().offset_from(original)));
    }
}

pub enum Frame {
    /// An entry frame from rust into the interperter.
    /// If this frame is popped the interperter should return to rust.
    Entry {
        // Size in number of registers of the frame.
        registers: u8,
        frame_offset: u8,
        open_upvalues: Vec<Gc<UpvalueObject>>,
    },
    /// A frame of a javascript function call.
    Call {
        // Size in number of registers of the frame.
        registers: u8,
        /// Number of try frames between the current call frame and the previous.
        frame_offset: u8,
        open_upvalues: Vec<Gc<UpvalueObject>>,
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
    pub reader: InstructionReader,
    /// The function object of the previous frame.
    pub function: Gc<Function>,
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

    cur_frame_size: u8,
    /// Amount of values allocated for the stack
    capacity: usize,
    /// Number of try frames between the current call frame and the previous.
    frame_offset: u8,
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
                open_upvalues: Vec::new(),
            });
            self.frame_offset = 0;
            self.frame = self.frame.add(self.cur_frame_size as usize);
            let new_stack = self.frame.add(registers as usize);
            while self.stack < new_stack {
                self.stack.write(Value::undefined());
                self.stack = self.stack.add(1);
            }
            self.cur_frame_size = registers;
        }
    }

    pub fn enter_call(
        &mut self,
        new_registers: u8,
        dst: u8,
        reader: InstructionReader,
        function: Gc<Function>,
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
            self.frames.push(Frame::Call {
                registers: self.cur_frame_size,
                data: CallFrameData {
                    dst,
                    reader,
                    function,
                },
                open_upvalues: Vec::new(),
                frame_offset: self.frame_offset,
            });
            self.frame_offset = 0;
            self.cur_frame_size = new_registers;
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

    pub unsafe fn unwind(&mut self) -> Option<TryFrameData> {
        loop {
            match self.frames.pop() {
                Some(Frame::Try { data }) => return Some(data),
                Some(Frame::Call {
                    registers,
                    frame_offset,
                    open_upvalues,
                    ..
                }) => {
                    open_upvalues.into_iter().for_each(|x| x.close());
                    self.restore_frame(registers, frame_offset);
                }
                Some(Frame::Entry {
                    registers,
                    frame_offset,
                    open_upvalues,
                }) => {
                    open_upvalues.into_iter().for_each(|x| x.close());
                    self.restore_frame(registers, frame_offset);
                    return None;
                }
                None => panic!("root frame was not an entry frame"),
            }
        }
    }

    pub unsafe fn pop(&mut self) -> Option<CallFrameData> {
        loop {
            match self.frames.pop() {
                Some(Frame::Try { .. }) => {}
                Some(Frame::Call {
                    registers,
                    data,
                    frame_offset,
                    open_upvalues,
                }) => {
                    open_upvalues.into_iter().for_each(|x| x.close());
                    self.restore_frame(registers, frame_offset);
                    return Some(data);
                }
                Some(Frame::Entry {
                    registers,
                    frame_offset,
                    open_upvalues,
                }) => {
                    open_upvalues.into_iter().for_each(|x| x.close());
                    self.restore_frame(registers, frame_offset);
                    self.frame_offset = 0;
                    return None;
                }
                None => panic!("tried to pop no existant stack"),
            }
        }
    }

    pub unsafe fn create_upvalue(&mut self, register: u8, gc: &GcArena) -> Gc<UpvalueObject> {
        let upvalue = gc.allocate(UpvalueObject {
            location: Cell::new(self.frame.add(register as usize)),
            closed: UnsafeCell::new(Value::empty()),
        });
        let frame_idx = self.frames.len() - self.frame_offset as usize - 1;
        match self.frames[frame_idx] {
            Frame::Call {
                ref mut open_upvalues,
                ..
            }
            | Frame::Entry {
                ref mut open_upvalues,
                ..
            } => {
                open_upvalues.push(upvalue);
            }
            _ => panic!(),
        }
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
                .expect("frame size execeded u32")
        }
    }

    #[inline]
    fn restore_frame(&mut self, registers: u8, frame_offset: u8) {
        unsafe {
            self.stack = self.frame;
            self.frame = self.frame.sub(registers as usize);
            self.cur_frame_size = registers;
            self.frame_offset = frame_offset
        }
    }

    fn grow(&mut self, capacity: usize) {
        unsafe {
            let capacity = capacity.next_power_of_two();
            if self.capacity == 0 {
                let layout = Layout::array::<Value>(capacity.max(8)).unwrap();
                let ptr = System.alloc(layout);
                if ptr.is_null() {
                    alloc::handle_alloc_error(layout);
                }
                self.root = NonNull::new_unchecked(ptr.cast());
                self.frame = self.root.as_ptr();
                self.stack = self.root.as_ptr();
            } else {
                let layout = Layout::array::<Value>(self.capacity).unwrap();
                let size = capacity * mem::size_of::<Value>();
                let ptr: *mut Value = System
                    .realloc(self.root.as_ptr().cast(), layout, size)
                    .cast();
                if ptr.is_null() {
                    alloc::handle_alloc_error(layout);
                }
                if ptr != self.root.as_ptr() {
                    self.stack = ptr.offset(self.stack.offset_from(self.root.as_ptr()));
                    self.frame = ptr.offset(self.frame.offset_from(self.root.as_ptr()));

                    // rebase all the open upvalue objects
                    let original = self.root.as_ptr();
                    for f in self.frames.iter_mut() {
                        match *f {
                            Frame::Entry {
                                ref mut open_upvalues,
                                ..
                            }
                            | Frame::Call {
                                ref mut open_upvalues,
                                ..
                            } => open_upvalues
                                .iter_mut()
                                .for_each(|x| x.rebase(original, ptr)),
                            _ => {}
                        }
                    }

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
        debug_assert!(register < self.cur_frame_size);
        unsafe { self.frame.add(register as usize).read() }
    }

    #[inline(always)]
    pub fn write(&mut self, register: u8, v: Value) {
        debug_assert!(register < self.cur_frame_size);
        unsafe { self.frame.add(register as usize).write(v) }
    }

    pub unsafe fn arguments<'a>(&self) -> Arguments<'a> {
        let size = self.stack.offset_from(self.frame);
        Arguments {
            end: self.frame.add(size as usize),
            frame: self.frame,
            marker: PhantomData,
        }
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
                Frame::Try { .. } => {}
                Frame::Call { ref data, .. } => {
                    data.reader.trace(ctx);
                    ctx.mark(data.function);
                }
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
