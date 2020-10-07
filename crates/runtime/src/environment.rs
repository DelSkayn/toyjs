use super::{gc::Trace, Ctx, Gc, JSValue};

pub struct Environment {
    parent: Option<Gc<Environment>>,
    slots: Box<[JSValue]>,
}

impl Environment {
    pub fn new(slots: u16, parent: Option<Gc<Environment>>) -> Self {
        Environment {
            parent,
            slots: vec![JSValue::undefined(); slots as usize].into_boxed_slice(),
        }
    }

    #[inline]
    pub fn get(&self, slot: u16) -> JSValue {
        self.slots[slot as usize]
    }

    #[inline]
    pub fn set(&mut self, slot: u16, v: JSValue) {
        self.slots[slot as usize] = v
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
        self.parent.map(|x| x.trace(ctx));
        self.slots.iter().for_each(|x| x.trace(ctx))
    }
}
