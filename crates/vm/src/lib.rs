#![allow(dead_code)]

pub mod cell;

pub mod gc2;

/*use gc::{GcRoot, Trace, Tracer};
use object::Object;

pub mod gc;
pub mod object;
pub mod value;

pub struct Realm<'rt, 'cell> {
    global: GcRoot<'rt, 'cell, Object<'rt, 'cell>>,
}

unsafe impl<'rt, 'cell> Trace<'rt, 'cell> for Realm<'rt, 'cell> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, trace: Tracer<'_, 'cell>) {
        trace.mark(*self.global);
    }
}

impl<'rt, 'cell> Realm<'rt, 'cell> {
    pub fn new(arena: gc::Arena<'rt, 'cell>) -> GcRoot<'rt, 'cell, Self> {
        todo!()
    }
}
*/
