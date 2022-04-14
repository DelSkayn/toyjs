use crate::gc::{Trace, Tracer};

pub struct Object {}

unsafe impl<'cell> Trace<'cell> for Object {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        false
    }

    fn trace(&self, _trace: Tracer<'_, 'cell>) {}
}
