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
        self.location.set(self.closed.get());
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

#[derive(Debug)]
pub enum Frame {
    /// An entry frame from rust into the interperter.
    /// If this frame is popped the interperter should return to rust.
    Entry {
        // Size in number of registers of the frame.
        // A u32 because entry frames can contain a lot of values
        // that where pushed by rust code
        registers: u32,
        open_upvalues: u16,
    },
    /// A frame of a javascript function call.
    Call {
        // Size in number of registers of the frame.
        registers: u8,
        /// Number of try frames between the current call frame and the previous.
        open_upvalues: u16,
        data: CallFrameData,
    },
    Try {
        data: TryFrameData,
    },
}

#[derive(Debug)]
pub struct CallFrameData {
    /// The register to put the return value into.
    pub dst: u8,
    /// The instruction reader
    pub instr: InstructionReader,
    pub ctx: ExecutionContext,
}

#[derive(Debug)]
pub struct TryFrameData {
    /// The destination to place to put the exception .
    pub dst: u8,
    /// The data of the this frame.
    pub ip_offset: usize,
}

/// The vm stack implementation.
pub struct Stack {
    /// Start of the stack array.
    root: Cell<NonNull<Value>>,
    /// points to the start of the current frame i.e. the first register in use.
    frame: Cell<*mut Value>,
    /// points one past the last value in use.
    stack: Cell<*mut Value>,

    frames: UnsafeCell<Vec<Frame>>,

    cur_frame_size: Cell<u32>,
    /// Amount of values allocated for the stack
    capacity: Cell<usize>,
    /// Number of try frames between the current call frame and the previous.
    open_upvalues: UnsafeCell<Vec<Gc<UpvalueObject>>>,
    frame_open_upvalues: Cell<u16>,
}

impl Stack {
    pub fn new() -> Self {
        let root = NonNull::dangling();
        Stack {
            frame: Cell::new(root.as_ptr()),
            stack: Cell::new(root.as_ptr()),
            root: Cell::new(root),
            frames: UnsafeCell::new(Vec::new()),
            cur_frame_size: Cell::new(0),
            capacity: Cell::new(0),
            open_upvalues: UnsafeCell::new(Vec::new()),
            frame_open_upvalues: Cell::new(0),
        }
    }

    /// Enter a base stack frame.
    /// When the frame returns execution should be
    pub fn enter(&self, registers: u8) {
        unsafe {
            let new_used = self.used() + registers as usize;
            if new_used > self.capacity.get() {
                self.grow(new_used);
            }
            (*self.frames.get()).push(Frame::Entry {
                registers: self.cur_frame_size.get(),
                open_upvalues: self.frame_open_upvalues.get(),
            });
            self.frame_open_upvalues.set(0);
            self.frame
                .set(self.frame.get().add(self.cur_frame_size.get() as usize));
            let new_stack = self.frame.get().add(registers as usize);
            while self.stack.get() < new_stack {
                self.stack.get().write(Value::undefined());
                self.stack.set(self.stack.get().add(1));
            }
            self.cur_frame_size.set(registers as u32);
        }
    }

    pub fn enter_call(
        &self,
        new_registers: u8,
        dst: u8,
        instr: InstructionReader,
        ctx: ExecutionContext,
    ) {
        unsafe {
            let new_used = self.used() + new_registers as usize;
            if new_used > self.capacity.get() {
                self.grow(new_used);
            }
            self.frame
                .set(self.frame.get().add(self.cur_frame_size.get() as usize));
            let mut cur = self.stack.get();
            self.stack.set(self.frame.get().add(new_registers as usize));
            while cur < self.stack.get() {
                cur.write(Value::undefined());
                cur = cur.add(1);
            }
            debug_assert!(
                self.cur_frame_size.get() < u8::MAX as u32,
                "frame size to big to fit in u8"
            );
            (*self.frames.get()).push(Frame::Call {
                registers: self.cur_frame_size.get() as u8,
                data: CallFrameData { dst, instr, ctx },
                open_upvalues: self.frame_open_upvalues.get(),
            });
            self.cur_frame_size.set(new_registers as u32);
            self.frame_open_upvalues.set(0);
        }
    }

    pub unsafe fn push_try(&self, dst: u8, ip_offset: usize) {
        (*self.frames.get()).push(Frame::Try {
            data: TryFrameData { dst, ip_offset },
        });
    }

    pub unsafe fn pop_try(&self) -> TryFrameData {
        let frame = (*self.frames.get()).pop();
        match frame {
            Some(Frame::Try { data }) => data,
            _ => panic!("tried to unguard stack which was not guarded"),
        }
    }

    pub unsafe fn unwind(&self, gc: &GcArena) -> Option<Result<TryFrameData, CallFrameData>> {
        match (*self.frames.get()).pop() {
            Some(Frame::Try { data }) => Some(Ok(data)),
            Some(Frame::Call {
                registers,
                open_upvalues,
                data,
            }) => {
                self.restore_frame(registers.into(), open_upvalues, gc);
                Some(Err(data))
            }
            Some(Frame::Entry {
                registers,
                open_upvalues,
            }) => {
                self.restore_frame(registers, open_upvalues, gc);
                None
            }
            None => panic!("root frame was not an entry frame"),
        }
    }

    pub unsafe fn pop(&self, gc: &GcArena) -> Option<CallFrameData> {
        loop {
            match (*self.frames.get()).pop() {
                Some(Frame::Try { .. }) => {}
                Some(Frame::Call {
                    registers,
                    data,
                    open_upvalues,
                }) => {
                    self.restore_frame(registers.into(), open_upvalues, gc);
                    return Some(data);
                }
                Some(Frame::Entry {
                    registers,
                    open_upvalues,
                }) => {
                    self.restore_frame(registers, open_upvalues, gc);
                    return None;
                }
                None => panic!("tried to pop non-existant stack"),
            }
        }
    }

    pub unsafe fn create_upvalue(&self, register: u8, gc: &GcArena) -> Gc<UpvalueObject> {
        let location = self.frame.get().add(register as usize);

        let upvalue_frame =
            (*self.open_upvalues.get()).len() - self.frame_open_upvalues.get() as usize;
        for &u in (*self.open_upvalues.get())[upvalue_frame..].iter() {
            if std::ptr::eq(u.location.get(), location) {
                return u;
            }
        }

        let upvalue = gc.allocate(UpvalueObject {
            location: Cell::new(location),
            closed: UnsafeCell::new(Value::empty()),
        });
        (*self.open_upvalues.get()).push(upvalue);
        self.frame_open_upvalues
            .set(self.frame_open_upvalues.get() + 1);
        upvalue
    }

    #[inline]
    pub fn push(&self, v: Value) {
        let used = self.used();
        if used + 1 > self.capacity.get() {
            self.grow(used + 1);
        }
        unsafe {
            self.stack.get().write(v);
            self.stack.set(self.stack.get().add(1));
        }
    }

    pub fn push_temp(&self, v: Value) {
        let used = self.used();
        if used + 1 > self.capacity.get() {
            self.grow(used + 1);
        }
        unsafe {
            self.stack.get().write(v);
            self.stack.set(self.stack.get().add(1));
            self.cur_frame_size.set(
                self.stack
                    .get()
                    .offset_from(self.frame.get())
                    .try_into()
                    .expect("frame size exceeded u32::MAX"),
            );
        }
    }

    #[inline]
    fn restore_frame(&self, registers: u32, open_upvalues: u16, gc: &GcArena) {
        unsafe {
            self.stack.set(self.frame.get());
            self.frame.set(self.frame.get().sub(registers as usize));
            self.cur_frame_size.set(registers);
            for _ in 0..self.frame_open_upvalues.get() {
                let up = (*self.open_upvalues.get()).pop().unwrap();
                gc.write_barrier(up);
                up.close();
            }
            self.frame_open_upvalues.set(open_upvalues);
        }
    }

    #[inline]
    unsafe fn rebase<T>(old: *mut T, new: *mut T, ptr: *mut T) -> *mut T {
        let offset = (ptr as isize) - (old as isize);
        new.cast::<u8>().offset(offset).cast()
    }

    #[cold]
    fn grow(&self, capacity: usize) {
        unsafe {
            let capacity = capacity.next_power_of_two().max(8);
            if self.capacity.get() == 0 {
                let layout = Layout::array::<Value>(capacity).unwrap();
                let ptr = alloc::alloc(layout);
                if ptr.is_null() {
                    alloc::handle_alloc_error(layout);
                }
                self.root.set(NonNull::new_unchecked(ptr.cast()));
                self.frame.set(self.root.get().as_ptr());
                self.stack.set(self.root.get().as_ptr());
            } else {
                let layout = Layout::array::<Value>(self.capacity.get()).unwrap();
                let size = Layout::array::<Value>(capacity).unwrap().size();
                let ptr: *mut Value =
                    alloc::realloc(self.root.get().as_ptr().cast(), layout, size).cast();
                if ptr.is_null() {
                    alloc::handle_alloc_error(layout);
                }
                if ptr != self.root.get().as_ptr() {
                    let original = self.root.get().as_ptr();
                    self.stack
                        .set(Self::rebase(original, ptr, self.stack.get()));
                    self.frame
                        .set(Self::rebase(original, ptr, self.frame.get()));
                    for up in &(*self.open_upvalues.get()) {
                        let loc = Self::rebase(original, ptr, up.location.get());
                        up.location.set(loc);
                    }
                    self.root.set(NonNull::new_unchecked(ptr));
                }
            }
            self.capacity.set(capacity);
        }
    }

    #[inline]
    pub fn frame_size(&self) -> usize {
        unsafe { self.stack.get().offset_from(self.frame.get()) as usize }
    }

    #[inline]
    fn used(&self) -> usize {
        unsafe { self.stack.get().offset_from(self.root.get().as_ptr()) as usize }
    }

    #[inline(always)]
    pub unsafe fn read(&self, register: u8) -> Value {
        debug_assert!((register as usize) < self.frame_size());
        self.frame.get().add(register as usize).read()
    }

    #[inline(always)]
    pub unsafe fn write(&self, register: u8, v: Value) {
        debug_assert!((register as usize) < self.frame_size());
        self.frame.get().add(register as usize).write(v)
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

        unsafe {
            for f in (*self.frames.get()).iter() {
                match *f {
                    Frame::Entry { .. } | Frame::Try { .. } => {}
                    Frame::Call { ref data, .. } => {
                        data.ctx.trace(ctx);
                    }
                }
            }
        }

        #[cfg(feature = "dump-gc-trace")]
        println!("TRACE: stack.stack");
        let mut cur = self.stack.get();
        while cur > self.root.get().as_ptr() {
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
                self.root.get().as_ptr().cast(),
                Layout::array::<Value>(self.capacity.get()).unwrap(),
            );
        }
    }
}
