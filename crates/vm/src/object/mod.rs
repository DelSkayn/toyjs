use crate::gc::{Gc, Trace, Tracer};

pub struct Object<'gc, 'cell> {
    prototype: Option<Gc<'gc, 'cell, Object<'gc, 'cell>>>,
}

unsafe impl<'gc, 'cell> Trace<'gc, 'cell> for Object<'gc, 'cell> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        false
    }

    fn trace(&self, trace: Tracer<'gc, 'cell>) {
        self.prototype.trace(trace);
    }
}
