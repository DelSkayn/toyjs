use super::Ctx;

pub unsafe trait Trace {
    fn needs_trace() -> bool
    where
        Self: Sized;

    fn trace(&self, ctx: Ctx);
}

macro_rules! impl_trace_primitive{
    ($($ty:ident,)*) => {
        $(
            unsafe impl Trace for $ty{
                fn needs_trace() -> bool{
                    false
                }

                fn trace(&self, ctx: Ctx){}
            }
        )*
    };
}

impl_trace_primitive!(
    bool, char, u8, u16, u32, u64, usize, i8, i16, i32, i64, f32, f64, isize, String, str,
);

unsafe impl<T: Trace> Trace for Box<T> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    fn trace(&self, ctx: Ctx) {
        (**self).trace(ctx)
    }
}

unsafe impl<T: Trace> Trace for Vec<T> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    fn trace(&self, ctx: Ctx) {
        self.iter().for_each(|x| x.trace(ctx))
    }
}

unsafe impl<T: Trace> Trace for std::rc::Rc<T> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    fn trace(&self, ctx: Ctx) {
        (**self).trace(ctx)
    }
}

unsafe impl Trace for () {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        false
    }

    fn trace(&self, _: Ctx) {}
}
