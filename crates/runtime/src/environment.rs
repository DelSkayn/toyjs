use super::{gc::Trace, Ctx, Gc, JSValue};
use std::{cell::Cell, rc::Rc};

pub struct Environment {
    parent: Option<Rc<Environment>>,
    slots: Box<[Cell<JSValue>]>,
}

impl Environment {
    pub fn new(slots: u32, parent: Option<Rc<Environment>>) -> Self {
        Environment {
            parent,
            slots: vec![Cell::new(JSValue::undefined()); slots as usize].into_boxed_slice(),
        }
    }

    #[inline]
    pub fn get(&self, slot: u32) -> JSValue {
        self.slots[slot as usize].get()
    }

    #[inline]
    pub fn set(&self, slot: u32, v: JSValue) {
        self.slots[slot as usize].set(v)
    }

    pub fn lookup_parent(&self, depth: u16) -> Rc<Environment> {
        debug_assert!(depth != 0);
        let mut parent = self.parent.as_ref().unwrap();
        for _ in 1..depth {
            parent = parent.parent.as_ref().unwrap()
        }
        parent.clone()
    }
}

unsafe impl Trace for Environment {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: Ctx) {
        self.parent.as_ref().map(|x| x.trace(ctx));
        self.slots.iter().for_each(|x| x.get().trace(ctx))
    }
}
