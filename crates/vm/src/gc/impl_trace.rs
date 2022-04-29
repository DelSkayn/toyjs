use super::{Gc, Rebind, Trace, Tracer};

macro_rules! impl_trace_primitive{
($($ty:ident,)*) => {
    $(
        unsafe impl Trace for $ty{
            fn needs_trace() -> bool{
                false
            }

            fn trace<'a>(&self, _t: Tracer<'a>){}
        }

        unsafe impl<'rt> Rebind<'rt> for $ty {
            type Output = $ty;
        }
    )*
};
}

impl_trace_primitive!(
    bool, char, u8, u16, u32, u64, usize, i8, i16, i32, i64, f32, f64, isize, String,
);

unsafe impl<'cell, T: Trace> Trace for Option<T> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    fn trace<'a>(&self, trace: Tracer<'a>) {
        if let Some(ref x) = *self {
            x.trace(trace);
        }
    }
}

unsafe impl<'cell, T: Trace> Trace for Box<T> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    fn trace<'a>(&self, trace: Tracer<'a>) {
        (**self).trace(trace);
    }
}

unsafe impl<'cell, T: Trace> Trace for [T] {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    fn trace<'a>(&self, trace: Tracer<'a>) {
        for t in self {
            t.trace(trace);
        }
    }
}

unsafe impl<'gc, 'cell, T: Trace> Trace for Gc<'gc, 'cell, T> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    fn trace<'a>(&self, trace: Tracer<'a>) {
        trace.mark(*self);
    }
}

unsafe impl<'gc, 'cell> Trace for Gc<'gc, 'cell, dyn Trace> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, trace: Tracer<'a>) {
        trace.mark_dynamic(*self);
    }
}

unsafe impl<'a, 'gc, 'cell, T: Rebind<'a>, R: Rebind<'a>> Rebind<'a> for Result<T, R> {
    type Output = Result<T::Output, R::Output>;
}
