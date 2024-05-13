use std::{cell::UnsafeCell, ptr::NonNull};

use bc::{ByteCode, FunctionId};
use dreck::{sys::UnsafeTrace, Gc, Trace};

use crate::value::Value;

type GcUpvalue<'gc, 'own> = Gc<'gc, 'own, Upvalue<'gc, 'own>>;
pub struct Upvalue<'gc, 'own> {
    /// Pointer to a value on the stack, or if closed to slot.
    ptr: NonNull<Value<'gc, 'own>>,
    /// Slot, default initiatized to empty, contains the value when upvalue was closed over.
    slot: Value<'gc, 'own>,
}

unsafe impl<'own> Trace<'own> for Upvalue<'gc, 'own> {
    type Gc<'r>
    where
        Self: Sized,
    = Upvalue<'r, 'own>;

    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, marker: dreck::Marker<'own, '_>) {
        // Slot only because the ptr is kept alive elsewhere.
        self.slot.trace(marker)
    }
}

impl<'gc, 'own> Upvalue<'gc, 'own> {
    pub fn new(ptr: NonNull<Value<'gc, 'own>>) -> Self {
        Self {
            ptr,
            slot: Value::empty(),
        }
    }

    // Mutable because pointers swap position, so gc needs a write barrier.
    pub unsafe fn close(&mut self) {
        unsafe {
            self.slot = self.ptr.as_ptr().read();
        }
    }
}

pub struct BcFunction<'gc, 'own> {
    /// Which function within the bytecode this is
    id: FunctionId,
    /// The bytecode containing the function.
    bc: Gc<'gc, 'own, ByteCode>,
    /// The upvalues of this function object.
    upvalues: Box<[GcUpvalue<'gc, 'own>]>,
    /// Inline caches used in bytecode.
    // TODO: Implement
    inlines: (),
}
