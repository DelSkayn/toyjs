use super::{Gc, Trace, Tracer};

macro_rules! impl_trace_primitive{
($($ty:ident,)*) => {
    $(
        unsafe impl<'gc,'cell> Trace<'gc,'cell> for $ty{
            fn needs_trace() -> bool{
                false
            }

            fn trace(&self, _t: Tracer<'gc,'cell>){}
        }
    )*
};
}

impl_trace_primitive!(
    bool, char, u8, u16, u32, u64, usize, i8, i16, i32, i64, f32, f64, isize, String, str,
);

unsafe impl<'gc, 'cell, T: Trace<'gc, 'cell>> Trace<'gc, 'cell> for Option<T> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    fn trace(&self, trace: Tracer<'gc, 'cell>) {
        if let Some(ref x) = *self {
            x.trace(trace);
        }
    }
}

unsafe impl<'gc, 'cell, T: Trace<'gc, 'cell>> Trace<'gc, 'cell> for [T] {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    fn trace(&self, trace: Tracer<'gc, 'cell>) {
        for t in self {
            t.trace(trace);
        }
    }
}

unsafe impl<'gc, 'cell, T: Trace<'gc, 'cell> + 'static> Trace<'gc, 'cell> for Gc<'gc, 'cell, T> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    fn trace(&self, trace: Tracer<'gc, 'cell>) {
        trace.mark(*self);
    }
}

unsafe impl<'gc, 'cell> Trace<'gc, 'cell> for Gc<'gc, 'cell, dyn Trace<'gc, 'cell>> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, trace: Tracer<'gc, 'cell>) {
        trace.mark_dynamic(*self);
    }
}
