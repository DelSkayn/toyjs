use super::{Gc, Trace, Tracer};

macro_rules! impl_trace_primitive{
($($ty:ident,)*) => {
    $(
        unsafe impl<'cell> Trace<'cell> for $ty{
            fn needs_trace() -> bool{
                false
            }

            fn trace<'a>(&self, _t: Tracer<'a,'cell>){}
        }

        impl<'gc,'cell> Gc<'gc,'cell,$ty>{
            pub unsafe fn rebind<'rt, T>(self,_: &'rt T) -> Gc<'rt,'cell, $ty>{
                std::mem::transmute(self)
            }
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

    fn trace<'a>(&self, trace: Tracer<'a, 'cell>) {
        if let Some(ref x) = *self {
            x.trace(trace);
        }
    }
}

unsafe impl<'cell, T: Trace<'cell>> Trace<'cell> for Box<T> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    fn trace<'a>(&self, trace: Tracer<'a, 'cell>) {
        (**self).trace(trace);
    }
}

unsafe impl<'cell, T: Trace<'cell>> Trace<'cell> for [T] {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    fn trace<'a>(&self, trace: Tracer<'a, 'cell>) {
        for t in self {
            t.trace(trace);
        }
    }
}

unsafe impl<'gc, 'cell, T: Trace<'cell>> Trace<'cell> for Gc<'gc, 'cell, T> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    fn trace<'a>(&self, trace: Tracer<'a, 'cell>) {
        trace.mark(*self);
    }
}

unsafe impl<'gc, 'cell> Trace<'cell> for Gc<'gc, 'cell, dyn Trace<'cell>> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, trace: Tracer<'a, 'cell>) {
        trace.mark_dynamic(*self);
    }
}
