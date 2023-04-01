use dreck::{Bound, Gc, Trace, Tracer};

use crate::value::Value;

pub type GcUpvalueObject<'gc, 'own> = Gc<'gc, 'own, UpvalueObject<'gc, 'own>>;

pub struct UpvalueObject<'gc, 'own> {
    pub(crate) location: *mut Value<'gc, 'own>,
    pub(crate) closed: Value<'gc, 'own>,
}

impl<'gc, 'own> UpvalueObject<'gc, 'own> {
    pub unsafe fn close(&mut self) {
        self.closed = self.location.read();
        self.location = &mut self.closed;
    }

    pub fn read(&self) -> Value<'gc, 'own> {
        unsafe { self.location.read() }
    }

    pub fn write(&mut self, v: Value<'_, 'own>) {
        unsafe { self.location.write(dreck::rebind(v)) }
    }
}

unsafe impl<'gc, 'own> Trace<'own> for UpvalueObject<'gc, 'own> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, trace: Tracer<'a, 'own>) {
        self.closed.trace(trace);
    }
}

unsafe impl<'a, 'gc, 'own> Bound<'a> for UpvalueObject<'gc, 'own> {
    type Rebound = UpvalueObject<'a, 'own>;
}
