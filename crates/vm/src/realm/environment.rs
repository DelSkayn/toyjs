use std::{
    cell::{Cell, UnsafeCell},
    pin::Pin,
};

use crate::{gc::Trace, Gc, Value};

pub struct UpValue {
    location: Cell<*mut Value>,
    closed: UnsafeCell<Value>,
}

impl UpValue {
    pub const unsafe fn open(location: *mut Value) -> Self {
        UpValue {
            location: Cell::new(location),
            closed: UnsafeCell::new(Value::empty()),
        }
    }

    #[inline]
    pub fn is_closed(&self) -> bool {
        unsafe { (*self.closed.get()) == Value::empty() }
    }

    pub unsafe fn close(self: Pin<&Self>) {
        (*self.closed.get()) = self.location.get().read();
        self.location.set(self.closed.get())
    }
}

unsafe impl Trace for UpValue {
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

pub struct Environment {
    parent: Option<Gc<Environment>>,
    upvalues: Box<[UpValue]>,
}

unsafe impl Trace for Environment {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        if let Some(x) = self.parent {
            ctx.mark(x)
        }
        self.upvalues.iter().for_each(|x| x.trace(ctx));
    }
}

impl Environment {
    pub fn root() -> Self {
        Environment {
            parent: None,
            upvalues: Box::new([]),
        }
    }

    pub unsafe fn close(&self) {}

    pub unsafe fn get_upvalue(&self, _slot: u8) -> UpValue {
        todo!()
    }

    pub unsafe fn get(&self, _slot: u8) -> Value {
        todo!()
    }

    pub unsafe fn set(&self, _slot: u8, _value: Value) {
        todo!()
    }
}
