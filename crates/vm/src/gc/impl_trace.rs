use super::{Gc, Trace, Tracer};

macro_rules! impl_trace_primitive{
($($ty:ident,)*) => {
    $(
        unsafe impl<'cell> Trace<'cell> for $ty{
            fn needs_trace() -> bool{
                false
            }

            fn trace(&self, _t: Tracer<'_,'cell>){}
        }
    )*
};
}

impl_trace_primitive!(
    bool, char, u8, u16, u32, u64, usize, i8, i16, i32, i64, f32, f64, isize, String, str,
);

unsafe impl<'cell, T: Trace<'cell>> Trace<'cell> for Option<T> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    fn trace(&self, trace: Tracer<'_, 'cell>) {
        if let Some(ref x) = *self {
            x.trace(trace);
        }
    }
}

unsafe impl<'cell, T: Trace<'cell>> Trace<'cell> for [T] {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    fn trace(&self, trace: Tracer<'_, 'cell>) {
        for t in self {
            t.trace(trace);
        }
    }
}

unsafe impl<'cell, T: Trace<'cell> + 'static> Trace<'cell> for Gc<'_, 'cell, T> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    fn trace(&self, trace: Tracer<'_, 'cell>) {
        trace.mark(*self);
    }
}

unsafe impl<'cell> Trace<'cell> for Gc<'_, 'cell, dyn Trace<'cell>> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, trace: Tracer<'_, 'cell>) {
        trace.mark_dynamic(*self);
    }
}
