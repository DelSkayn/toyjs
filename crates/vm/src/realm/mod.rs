use crate::{
    cell::CellOwner,
    gc::{self, Gc, Trace, Tracer},
    object::ObjectKind,
    GcObject, Object, Value,
};

use self::stack::Stack;

mod stack;

pub struct ExecutionContext<'gc, 'cell> {
    pub function: GcObject<'gc, 'cell>,
    pub this: Value<'gc, 'cell>,
    pub new_target: Value<'gc, 'cell>,
}

pub struct Realm<'gc, 'cell> {
    global: GcObject<'gc, 'cell>,
    stack: Stack<'gc, 'cell>,
}

unsafe impl<'gc, 'cell> Trace<'cell> for Realm<'gc, 'cell> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, trace: Tracer<'a, 'cell>) {
        trace.mark(self.global);
    }
}

impl<'gc, 'cell> Realm<'gc, 'cell> {
    pub fn new<'rt>(arena: &'gc gc::Arena<'rt, 'cell>) -> Self {
        let global = arena.add(Object::new(None, ObjectKind::Ordinary));
        let stack = Stack::new();
        Realm { global, stack }
    }
}

impl<'gc, 'cell> Gc<'gc, 'cell, Realm<'gc, 'cell>> {
    #[inline]
    pub fn global(&self, owner: &CellOwner<'cell>) -> GcObject<'gc, 'cell> {
        self.borrow(owner).global
    }

    #[inline]
    pub unsafe fn rebind<'rt, T>(self, _: &'rt T) -> Gc<'rt, 'cell, Realm<'rt, 'cell>> {
        std::mem::transmute(self)
    }
}
