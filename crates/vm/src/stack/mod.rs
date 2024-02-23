//! The implementation of the stack
//!
//! # Glossary
//! - Frame
//!     A number of registers on the stack which the bytecode of a function uses.
//! - Entry frame
//!     A frame pushed onto the stack when entering into the vm. This happens when calling the root
//!     function of a script for instance or when a rust function calls a javascript function.
//! - Error frame
//!     A frame pushed onto the stack by a try statement. Used during unwinding to find where
//!     execution should resume
//! - Stack pointer
//!     Pointer pointing to the first register of the current frame.
//! - Top pointer
//!     Pointer pointing to the first register past the end of the current frame.
//! - Root pointer
//!     Pointer pointing to the start of the stack.
//!
//!
//! # The call frame
//!
//! A function has both it's registers and its arguments on the stack.
//! Whenever a function calls the other function. It will place the arguments on the stack in the
//! right order. Arguments are put in reverse order with the last argument being the first on the
//! stack. The last two arguments are always the this object and the function object of the
//! function called.
//! When the function is called the stack will allocate a number of registers equal to the amount
//! required by the function as described in the bytecode. The call pointer will then point to the
//! first register. The function object and the this value are then at offset -1 and -2
//! respectively.
//!
//! ```text             
//!  REG INDEX:         -4      -3      -2      -1       0       1                    n
//!  ..══════════════╦═══════╦═══════╦═══════╦═══════╦═══════╦═══════╦══..   ..═══╦═══════╦═════..
//!   ...other frame ║ arg 1 ║ arg 0 ║ this  ║ func  ║ reg 0 ║ reg 1 ║  ..rest..  ║ reg n ║ unused
//!  ..══════════════╩═══════╩═══════╩═══════╩═══════╩═══════╩═══════╩══..   ..═══╩═══════╩═════..
//!                                                      △                                    △
//!  ◁ root pointer at the start           stack pointer ╝                        top pointer ╝
//!                                             
//! ```

use core::fmt;
use std::{num::NonZeroU32, ptr::NonNull};

use bc::{ByteCodeReader, Reg};
use common::{slow_assert, slow_assert_ne};
use dreck::{Marker, Trace};

mod buffer;

use self::buffer::RawBuffer;
use crate::value::Value;

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
    /// The size of the frame
    size: u32,
    /// The number of upvalues in this frame.
    upvalues: u32,
    /// The reader from the previous frame.
    /// Can be null if this was an entry frame.
    reader: Option<ByteCodeReader<'static>>,
}

// A struct which contains information about the current stack frame.
#[derive(Clone, Copy)]
pub struct ErrorFrameInfo {
    // Instruction offset from the start of the current active function to jump to.
    offset: u32,
    // Offset into the stack to the previous error stack value if it exists.
    prev: Option<NonZeroU32>,
}

pub struct Stack<'gc, 'own> {
    // The javascript values making up the statek
    values: RawBuffer<Value<'gc, 'own>>,
    /// Points to the start of current function frame.
    stack: NonNull<Value<'gc, 'own>>,
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
            stack: call,
            top,
            frames,
            frame,
            error: NonNull::dangling(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Result<Self, StackSizeError> {
        let mut res = Self::new();
        res.reserve(capacity)?;
        Ok(res)
    }

    /// Returns the amount of stack values in use.
    pub fn size(&self) -> usize {
        unsafe { self.top.as_ptr().offset_from(self.values.end().as_ptr()) as usize }
    }

    /// Returns the amount of stack values in use.
    pub fn frame_size(&self) -> usize {
        unsafe { self.top.as_ptr().offset_from(self.stack.as_ptr()) as usize }
    }

    /// Make sure the stack has enough memory for atleast amount extra values.
    pub fn reserve(&mut self, amount: usize) -> Result<(), StackSizeError> {
        unsafe {
            let offset = self.values.end().as_ptr().offset_from(self.stack.as_ptr()) as usize;
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
                    self.stack = NonNull::new_unchecked(
                        to.as_ptr()
                            .offset(self.stack.as_ptr().offset_from(from.as_ptr())),
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
            let cap = self.frames.capacity().max(4);
            self.frames.grow(cap.next_power_of_two(), |from, to| {
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

    unsafe fn frame_info(&self) -> &CallFrameInfo {
        &self.frame.as_ref().call
    }

    unsafe fn frame_info_mut(&mut self) -> &mut CallFrameInfo {
        &mut self.frame.as_mut().call
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
        self.stack.as_ptr().offset(reg.0 as isize).read()
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
        self.stack.as_ptr().offset(reg.0 as isize).write(value)
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
        self.stack.as_ptr().offset(tgt as isize).read()
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
        self.stack.as_ptr().offset(tgt as isize).write(value)
    }

    pub unsafe fn push_entry_frame(&mut self, size: u32) -> Result<(), StackSizeError> {
        // Reserve for atleast size amount of registers
        // + 1 for the this value .
        // + 1 for the current function.
        self.reserve(size as usize)?;

        self.frame = self.push_info(FrameInfo {
            call: CallFrameInfo {
                size: self.top.as_ptr().offset_from(self.stack.as_ptr()) as u32,
                upvalues: 0,
                reader: None,
            },
        });

        // Initialize the frame.
        self.stack = NonNull::new_unchecked(self.top.as_ptr());
        self.top = NonNull::new_unchecked(self.top.as_ptr().add(size as usize));
        // We need to write values into the stack, cause they can contain no longer reachable
        // pointers.
        std::ptr::write_bytes(self.stack.as_ptr(), 0, size as usize);
        Ok(())
    }

    pub unsafe fn pop_entry_frame(&mut self) {
        slow_assert!(self.frame > self.error, "Can only pop the top frame");
        slow_assert_ne!(
            self.frame,
            self.frames.root(),
            "Tried to pop from empty stack"
        );

        let size = self.frame.as_ref().call.size;

        // TODO arg check limit, prevent u32 overflow.
        self.top = self.stack;
        self.stack = NonNull::new_unchecked(self.stack.as_ptr().sub(size as usize));

        self.frame = NonNull::new_unchecked(self.frame.as_ptr().sub(1));
        if self.frame.as_ptr() == self.error.as_ptr() {
            let mut cur_error = self.error;
            slow_assert!(cur_error.as_ref().error.prev.is_some());
            // If the current offset is 1 the next frame is also an error so keep searching.
            while cur_error.as_ref().error.prev.unwrap_unchecked() == NonZeroU32::new_unchecked(1) {
                cur_error = NonNull::new_unchecked(cur_error.as_ptr().sub(1));
            }
            self.frame = NonNull::new_unchecked(cur_error.as_ptr().sub(1));
        }
    }

    /// Push a value onto the frame increasing the size of the stack.
    pub fn push(&mut self, value: Value<'gc, 'own>) -> Result<(), StackSizeError> {
        self.reserve(1)?;

        if self.frame_size() == i32::MAX as usize {
            return Err(StackSizeError);
        }

        unsafe {
            self.top.as_ptr().write(value);
        }
        Ok(())
    }

    /// Pop value of the stack
    pub unsafe fn pop(&mut self) -> Value<'gc, 'own> {
        slow_assert!(self.frame_size() >= 1);
        todo!()
    }

    /// Push a catch frame onto the stack.
    pub fn push_catch(&mut self, _offset: u32) -> Result<(), StackSizeError> {
        todo!()
    }
}

impl<'gc, 'own> Default for Stack<'gc, 'own> {
    fn default() -> Self {
        Self::new()
    }
}
