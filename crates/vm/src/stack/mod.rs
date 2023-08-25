//! The implementation of the stack
//!
//!
//! # The call frame
//!
//! ```text             
//!  REG INDEX:         -4      -3      -2      -1       0       1                    n
//!  ..══════════════╦═══════╦═══════╦═══════╦═══════╦═══════╦═══════╦══..   ..═══╦═══════╦═════..
//!   ...other frame ║ arg 1 ║ arg 0 ║ this  ║ func  ║ reg 0 ║ reg 1 ║  ..rest..  ║ reg n ║ unused
//!  ..══════════════╩═══════╩═══════╩═══════╩═══════╩═══════╩═══════╩══..   ..═══╩═══════╩═════..
//!                                                      △                                    △
//!  ◁ root pointer at the start            call pointer ╝                        top pointer ╝
//!                                             
//! ```

use bc::{ByteCodeReader, Reg};
use common::{slow_assert, slow_assert_ne};
use core::fmt;
use dreck::{Marker, Trace};
use std::{
    mem::MaybeUninit,
    num::NonZeroU32,
    ptr::{self, NonNull},
};

mod buffer;

use crate::{object::GcObject, value::Value};

use self::buffer::RawBuffer;

#[derive(Debug)]
pub struct StackSizeError;

impl fmt::Display for StackSizeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Stack sizes exceeded limit")
    }
}

union FrameInfo {
    call: CallFrameInfo,
    error: ErrorFrameInfo,
}

// A struct which contains information about the current stack frame.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct CallFrameInfo {
    // The size of the frame
    size: u32,
    // the number of args
    args: u32,
    // The reader from the previous frame.
    // Optionally uninitialized if this is an entry frame.
    reader: MaybeUninit<ByteCodeReader<'static>>,
}

// A struct which contains information about the current stack frame.
#[derive(Clone, Copy)]
pub struct ErrorFrameInfo {
    // Instruction offset from the start of the current active function to jump to.
    offset: u32,
    // Offset into the stack to the previous error stack value if it exists.
    prev: Option<NonZeroU32>,
    // Offset from the current error frame to the previous call frame.
    prev_frame: NonZeroU32,
}

pub struct Stack<'gc, 'own> {
    // The javascript values making up the statek
    values: RawBuffer<Value<'gc, 'own>>,
    /// Points to the start of current function frame.
    call: NonNull<Value<'gc, 'own>>,
    /// Points to the top of the stack to one past the last valid value of the current frame.
    top: NonNull<Value<'gc, 'own>>,

    /// Buffer containing info not stored as a javascript value.
    frames: RawBuffer<FrameInfo>,
    // Points to the start of current function frame.
    frame: NonNull<FrameInfo>,
    // Points to the current error frame.
    error: NonNull<FrameInfo>,
}

unsafe impl<'gc, 'own> Trace<'own> for Stack<'gc, 'own> {
    type Gc<'r> = Stack<'r, 'own>;

    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, _marker: Marker<'own, '_>) {
        todo!()
    }
}

impl<'gc, 'own> Stack<'gc, 'own> {
    // Smallest size the stack will allocate.
    pub const MIN_ALLOC_SIZE: usize = 8;
    pub const MAX_STACK_SIZE: usize = u32::MAX as usize;
    pub const MAX_DEPTH: usize = u32::MAX as usize;

    pub fn new() -> Self {
        let values = RawBuffer::new();
        let call = values.root();
        let top = values.root();

        let frames = RawBuffer::new();
        let frame = frames.root();
        Stack {
            values,
            call,
            top,
            frames,
            frame,
            error: NonNull::dangling(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        let mut res = Self::new();
        res.reserve(capacity);
        res
    }

    /// Returns the amount of stack values in use.
    pub fn size(&self) -> usize {
        unsafe { self.top.as_ptr().offset_from(self.values.end().as_ptr()) as usize }
    }

    /// Make sure the stack has enough memory for atleast amount extra values.
    pub fn reserve(&mut self, amount: usize) -> Result<(), StackSizeError> {
        unsafe {
            let offset = self.values.end().as_ptr().offset_from(self.call.as_ptr()) as usize;
            if offset < amount {
                let current_size =
                    self.top.as_ptr().offset_from(self.values.end().as_ptr()) as usize;
                let new_size = (current_size + amount)
                    .next_power_of_two()
                    .max(Self::MIN_ALLOC_SIZE);
                if new_size > Self::MAX_STACK_SIZE {
                    return Err(StackSizeError);
                }
                self.values.grow(new_size, |from, to| {
                    self.call = NonNull::new_unchecked(
                        to.as_ptr()
                            .offset(self.call.as_ptr().offset_from(from.as_ptr())),
                    );
                    self.top = NonNull::new_unchecked(
                        to.as_ptr()
                            .offset(self.top.as_ptr().offset_from(from.as_ptr())),
                    );
                });
            }
        }
        Ok(())
    }

    unsafe fn push_info(&mut self, frame: FrameInfo) -> NonNull<FrameInfo> {
        let last = self.frame.max(self.error);
        if last == self.frames.end() {
            let mut last = last;
            let cap = dbg!(self.frames.capacity().max(4));
            self.frames.grow(cap.next_power_of_two(), |from, to| {
                dbg!(self.frame);
                self.error = NonNull::new_unchecked(
                    to.as_ptr()
                        .offset(self.error.as_ptr().offset_from(from.as_ptr())),
                );
                self.frame = NonNull::new_unchecked(
                    to.as_ptr()
                        .offset(self.frame.as_ptr().offset_from(from.as_ptr())),
                );
                last = self.frame.max(self.error);
            });
            last.as_ptr().write(frame);
            last
        } else {
            last.as_ptr().write(frame);
            last
        }
    }

    unsafe fn frame_info(&self) -> &FrameInfo {
        self.frame.as_ref()
    }

    #[inline(always)]
    pub unsafe fn read(&self, reg: Reg) -> Value<'gc, 'own> {
        // read must be inside current frame.
        slow_assert!({
            let frame = self.frame_info();
            // +2 for stack info
            reg.0 as isize >= -(frame.args as isize + 2) && (reg.0 as u8) < frame.size
        });
        // can't write to the stack info.
        self.call.as_ptr().offset(reg.0 as isize).read()
    }

    #[inline(always)]
    pub unsafe fn write<'l>(&self, reg: Reg, value: Value<'l, 'own>) {
        // read must be inside current frame.
        slow_assert!({
            let frame = self.frame_info();
            // +2 for stack info and function
            reg.0 as isize >= -(frame.args as isize + 2) && (reg.0 as u8) < frame.size
        });
        // the function object should not be overwritten.
        slow_assert_ne!(reg, Reg::function_reg());
        self.call.as_ptr().offset(reg.0 as isize).write(value)
    }

    #[inline(always)]
    pub unsafe fn long_read(&self, tgt: i32) -> Value<'gc, 'own> {
        // read must be inside current frame.
        slow_assert!({
            let frame = self.frame_info();
            // +2 for stack info
            reg.0 as isize > -(frame.args as isize + 2) && (reg.0 as isize) < frame.size as isize
        });
        // can't write to the stack info.
        self.call.as_ptr().offset(tgt as isize).read()
    }

    #[inline(always)]
    pub unsafe fn long_write<'l>(&self, tgt: i32, value: Value<'l, 'own>) {
        // read must be inside current frame.
        slow_assert!({
            let frame = self.frame_info();
            // +2 for stack info and function
            reg.0 as isize > -(frame.args as isize + 2) && (reg.0 as isize) < frame.size as isize
        });
        // can't write to the stack info.
        self.call.as_ptr().offset(tgt as isize).write(value)
    }

    pub unsafe fn push_entry_frame(
        &mut self,
        args: u32,
        size: u32,
        function: GcObject<'gc, 'own>,
        this: Value<'gc, 'own>,
    ) -> Result<(), StackSizeError> {
        // Reserve for atleast size amount of registers
        // + 1 for the this value .
        // + 1 for the current function.
        self.reserve(size as usize + 2)?;
        unsafe {
            self.frame = self.push_info(FrameInfo {
                call: CallFrameInfo {
                    size,
                    args,
                    reader: MaybeUninit::uninit(),
                },
            })
        };

        // Initialize the frame.
        self.call = NonNull::new_unchecked(self.top.as_ptr().add(2));
        self.top = NonNull::new_unchecked(self.top.as_ptr().add(size as usize + 2));
        self.write(Reg::this_reg(), this);
        self.write(Reg::function_reg(), function.into());
        // We need to write values into the stack, cause they can contain no longer reachable
        // pointers.
        std::ptr::write_bytes(self.call.as_ptr(), 0, size as usize);
        Ok(())
    }

    pub unsafe fn pop_frame(&mut self) {}

    pub fn push(&mut self, value: Value<'gc, 'own>) -> Result<(), StackSizeError> {
        self.reserve(1)?;
        todo!()
    }

    /// Pop value of the stack
    pub unsafe fn pop(&mut self) -> Value<'gc, 'own> {
        todo!()
    }

    /// Push a catch frame onto the stack.
    pub fn push_catch(&mut self, offset: u32) -> Result<(), StackSizeError> {
        todo!()
    }
}
