mod upvalue;
use std::{
    alloc::{self, Layout},
    ptr::NonNull,
};

use dreck::{rebind, Bound, Gc, Owner, Root, Trace, Tracer};
pub use upvalue::{GcUpvalueObject, UpvalueObject};

use crate::{object::GcObject, value::Value};

use super::InstructionReader;

pub enum FrameType<'gc, 'own> {
    /// A frame from a rust function into the vm function,
    Entry {
        /// The size of the new frame
        size: usize,
    },
    /// A frame from a vm function to a vm function,
    Internal {
        ///The current instruction reader.
        reader: InstructionReader<'gc, 'own>,
        /// The function object
        function: GcObject<'gc, 'own>,
        /// The size of the new frame
        size: u8,
        /// The register to store the result into
        dst: u8,
    },
}

unsafe impl<'gc, 'own> Trace<'own> for FrameType<'gc, 'own> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, tracer: Tracer<'a, 'own>) {
        match *self {
            Self::Internal {
                ref reader,
                function,
                ..
            } => {
                reader.trace(tracer);
                tracer.mark(function);
            }
            Self::Entry { .. } => {}
        }
    }
}

unsafe impl<'from, 'to, 'own> Bound<'to> for FrameType<'from, 'own> {
    type Rebound = FrameType<'to, 'own>;
}

enum Frame<'gc, 'own> {
    Entry {
        size: usize,
    },
    Internal {
        reader: InstructionReader<'gc, 'own>,
        function: GcObject<'gc, 'own>,
        size: usize,
        upvalues: u16,
        dst: u8,
    },
}

unsafe impl<'gc, 'own> Trace<'own> for Frame<'gc, 'own> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, tracer: Tracer<'a, 'own>) {
        match *self {
            Self::Internal {
                ref reader,
                function,
                ..
            } => {
                reader.trace(tracer);
                tracer.mark(function);
            }
            Self::Entry { .. } => {}
        }
    }
}

unsafe impl<'from, 'to, 'own> Bound<'to> for Frame<'from, 'own> {
    type Rebound = Frame<'to, 'own>;
}

pub type GcStack<'gc, 'own> = Gc<'gc, 'own, Stack<'gc, 'own>>;

pub struct Stack<'gc, 'own> {
    root: NonNull<Value<'gc, 'own>>,
    // The frame pointer, points to the start of the current frame.
    frame: *mut Value<'gc, 'own>,
    // The stack pointer, points to one past the last in use value on the stack.
    stack: *mut Value<'gc, 'own>,

    frames: Vec<Frame<'gc, 'own>>,

    capacity: usize,

    pub register_amount: usize,

    upvalues: Vec<GcUpvalueObject<'gc, 'own>>,

    frame_upvalues: u16,
}

impl<'gc, 'own> Stack<'gc, 'own> {
    const MIN_CAPACITY: usize = 8;
    // Rust has a long standing but where match blocks stack usages grows linearly with the amount
    // of matches
    // This means that with the debug profile the depth the vm can goto is really limited.
    pub fn new() -> Self {
        let root = NonNull::dangling();
        Stack {
            frame: root.as_ptr(),
            stack: root.as_ptr(),
            root,
            capacity: 0,

            frames: Vec::new(),

            register_amount: 0,

            upvalues: Vec::new(),
            frame_upvalues: 0,
        }
    }

    #[inline]
    unsafe fn rebase<T>(old: *mut T, new: *mut T, ptr: *mut T) -> *mut T {
        let offset = (ptr as isize) - (old as isize);
        new.cast::<u8>().offset(offset).cast()
    }

    #[cold]
    unsafe fn grow(this: GcStack<'gc, 'own>, owner: &mut Owner<'own>, new_capacity: usize) {
        let this_bor = this.unsafe_borrow_mut(owner);
        let capacity = this_bor.capacity;
        if capacity == 0 {
            let new_capacity = new_capacity.next_power_of_two().max(Self::MIN_CAPACITY);
            let layout = Layout::array::<Value>(new_capacity).unwrap();
            let ptr = alloc::alloc(layout);
            if ptr.is_null() {
                alloc::handle_alloc_error(layout);
            }
            this_bor.root = NonNull::new_unchecked(ptr.cast());
            this_bor.frame = this_bor.root.as_ptr();
            this_bor.stack = this_bor.root.as_ptr();
            this_bor.capacity = new_capacity;
        } else {
            let new_capacity = new_capacity.next_power_of_two();
            let layout = Layout::array::<Value>(capacity).unwrap();
            let size = Layout::array::<Value>(new_capacity).unwrap().size();
            let ptr = alloc::realloc(this_bor.root.as_ptr().cast(), layout, size);
            if ptr.is_null() {
                alloc::handle_alloc_error(layout);
            }
            let root_ptr = this_bor.root.as_ptr();
            if ptr.cast() != root_ptr {
                let original: *mut u8 = this_bor.root.as_ptr().cast();
                this_bor.stack = Self::rebase(original.cast(), ptr.cast(), this_bor.stack);
                this_bor.frame = Self::rebase(original.cast(), ptr.cast(), this_bor.frame);
                // Stack has unique accesso
                // TODO find a better way to do this within dreck
                this_bor.root = NonNull::new_unchecked(ptr.cast());
                for idx in 0..this.borrow(owner).upvalues.len() {
                    let up: Gc<UpvalueObject> = this.borrow(owner).upvalues[idx];
                    let loc: *mut u8 = up.borrow(owner).location.cast();
                    let loc = Self::rebase(original.cast(), ptr.cast(), loc);
                    up.unsafe_borrow_mut(owner).location = loc.cast();
                }
            }
        }
    }

    fn reserve_additional(
        this: GcStack<'gc, 'own>,
        owner: &mut Owner<'own>,
        extra_capacity: usize,
    ) {
        unsafe {
            let used = this
                .borrow(owner)
                .frame
                .offset_from(this.borrow(owner).root.as_ptr()) as usize;
            if used + extra_capacity as usize > this.borrow(owner).capacity {
                Stack::grow(this, owner, used + extra_capacity)
            }
        }
    }

    pub fn push_frame(
        this: GcStack<'gc, 'own>,
        owner: &mut Owner<'own>,
        root: &Root<'own>,
        frame: FrameType<'_, 'own>,
    ) {
        unsafe {
            let size = match frame {
                FrameType::Entry { size } => {
                    Stack::reserve_additional(this, owner, size as usize);
                    let register_amount = this.borrow(owner).register_amount;
                    this.unsafe_borrow_mut(owner).frames.push(Frame::Entry {
                        size: register_amount,
                    });
                    size
                }
                FrameType::Internal {
                    size,
                    reader,
                    function,
                    dst,
                } => {
                    Stack::reserve_additional(this, owner, size as usize);
                    let register_amount = this.borrow(owner).register_amount;
                    let frame_upvalues = this.borrow(owner).frame_upvalues;
                    // Possibly added a new pointer so borrowing unsafe is not allowed here.
                    this.borrow_mut(owner, root)
                        .frames
                        .push(dreck::rebind(Frame::Internal {
                            dst,
                            function,
                            reader,
                            size: register_amount,
                            upvalues: frame_upvalues,
                        }));
                    size as usize
                }
            };
            let this = this.unsafe_borrow_mut(owner);
            this.frame = this.frame.add(this.register_amount);
            this.register_amount = size;
            let new_stack = this.frame.add(size);
            while this.stack < new_stack {
                this.stack.write(Value::undefined());
                this.stack = this.stack.add(1);
            }
        }
    }

    pub fn pop_frame<'l>(
        this: GcStack<'gc, 'own>,
        owner: &mut Owner<'own>,
        root: &'l Root<'own>,
    ) -> FrameType<'l, 'own> {
        unsafe {
            let this_borrow = this.borrow_mut(owner, root);
            this_borrow.stack = this_borrow.frame;
            match this_borrow
                .frames
                .pop()
                .expect("tried to pop non-existant frame")
            {
                Frame::Internal {
                    reader,
                    size,
                    upvalues,
                    function,
                    dst,
                } => {
                    let res = rebind!(
                        root,
                        FrameType::Internal {
                            reader,
                            dst,
                            function,
                            size: this_borrow.register_amount as u8,
                        }
                    );

                    this_borrow.frame = this_borrow.frame.sub(size);
                    this_borrow.register_amount = size;

                    let new_closed_upvalues = this_borrow.frame_upvalues;
                    this_borrow.frame_upvalues = upvalues;
                    for _ in 0..new_closed_upvalues {
                        let upvalue = this.unsafe_borrow_mut(owner).upvalues.pop().unwrap();
                        let upvalue = rebind!(root, upvalue);
                        upvalue.borrow_mut(owner, root).close();
                    }

                    res
                }
                Frame::Entry { size } => {
                    let res = FrameType::Entry {
                        size: this_borrow.register_amount,
                    };

                    this_borrow.frame = this_borrow.frame.sub(size);
                    this_borrow.register_amount = size;
                    res
                }
            }
        }
    }

    #[inline(always)]
    pub unsafe fn read_arg(&self, reg: u32) -> Option<Value<'gc, 'own>> {
        let ptr = self.frame.add(reg as usize);
        if ptr < self.stack {
            Some(ptr.read())
        } else {
            None
        }
    }

    #[inline(always)]
    pub unsafe fn read(&self, reg: u8) -> Value<'gc, 'own> {
        debug_assert!((reg as usize) < self.register_amount);
        self.frame.add(reg as usize).read()
    }

    #[inline(always)]
    pub unsafe fn write<'l>(&mut self, reg: u8, v: Value<'l, 'own>) {
        debug_assert!((reg as usize) < self.register_amount);
        self.frame.add(reg as usize).write(dreck::rebind(v));
    }

    pub unsafe fn push<'l>(this: GcStack<'gc, 'own>, owner: &mut Owner<'own>, v: Value<'l, 'own>) {
        Self::reserve_additional(this, owner, 1);
        let this = this.unsafe_borrow_mut(owner);
        this.stack.write(v);
        this.stack = this.stack.add(1);
    }
}

impl<'gc, 'own> Drop for Stack<'gc, 'own> {
    fn drop(&mut self) {
        if self.capacity != 0 {
            let layout = Layout::array::<Value>(self.capacity).unwrap();
            unsafe {
                alloc::dealloc(self.root.as_ptr().cast(), layout);
            }
        }
    }
}

unsafe impl<'gc, 'own> Trace<'own> for Stack<'gc, 'own> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, tracer: Tracer<'a, 'own>) {
        // Upvalues dont need to be traced since all of them are open and contain no pointer
        // values.
        unsafe {
            let mut cur = self.root.as_ptr();
            while cur < self.stack {
                cur.read().trace(tracer);
                cur = cur.add(1);
            }
            for f in &self.frames {
                f.trace(tracer);
            }
        }
    }
}

unsafe impl<'from, 'to, 'own> Bound<'to> for Stack<'from, 'own> {
    type Rebound = Stack<'to, 'own>;
}
