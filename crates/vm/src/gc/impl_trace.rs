use super::{Gc, Rebind, Trace, Tracer};

macro_rules! impl_trace_primitive{
($($ty:ident,)*) => {
    $(
        unsafe impl Trace for $ty{
            fn needs_trace() -> bool{
                false
            }

            fn trace(&self, _t: Tracer){}
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

macro_rules! impl_trace_tuple{
    ($($ty:ident),*) => {
            #[allow(non_snake_case)]
            #[allow(unused_variables)]
            unsafe impl<$($ty:Trace,)*> Trace for ($($ty,)*){
                fn needs_trace() -> bool{
                    false $(|| $ty::needs_trace())*
                }

                fn trace(&self, t: Tracer){
                    let ($(ref $ty,)*) = self;
                    $($ty.trace(t);)*
                }
            }

            unsafe impl<'rt, $($ty: Rebind<'rt>,)*> Rebind<'rt> for ($($ty,)*){
                type Output = ($($ty::Output,)*);
            }
    };
}

impl_trace_tuple!();
impl_trace_tuple!(A);
impl_trace_tuple!(A, B);
impl_trace_tuple!(A, B, C);
impl_trace_tuple!(A, B, C, D);
impl_trace_tuple!(A, B, C, D, E);
impl_trace_tuple!(A, B, C, D, E, F);
impl_trace_tuple!(A, B, C, D, E, F, G);
impl_trace_tuple!(A, B, C, D, E, F, G, H);

unsafe impl<'cell, T: Trace> Trace for Option<T> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    fn trace(&self, trace: Tracer) {
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

    fn trace(&self, trace: Tracer) {
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

    fn trace(&self, trace: Tracer) {
        for t in self {
            t.trace(trace);
        }
    }
}

unsafe impl<'cell, T: Trace> Trace for Vec<T> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    fn trace(&self, trace: Tracer) {
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

    fn trace(&self, trace: Tracer) {
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

    fn trace(&self, trace: Tracer) {
        trace.mark_dynamic(*self);
    }
}

unsafe impl<'a, 'b, 'gc, 'cell, T: Rebind<'a>> Rebind<'a> for &'b T
where
    T::Output: 'b,
{
    type Output = &'b T::Output;
}

unsafe impl<'a, 'b, 'gc, 'cell, T: Rebind<'a>> Rebind<'a> for &'b mut T
where
    T::Output: 'b,
{
    type Output = &'b mut T::Output;
}

unsafe impl<'a, 'b, 'gc, 'cell, T: Rebind<'a>> Rebind<'a> for std::pin::Pin<&'b T>
where
    T::Output: 'b,
{
    type Output = std::pin::Pin<&'b T::Output>;
}

unsafe impl<'a, 'b, 'gc, 'cell, T: Rebind<'a>> Rebind<'a> for std::pin::Pin<&'b mut T>
where
    T::Output: 'b,
{
    type Output = std::pin::Pin<&'b mut T::Output>;
}

unsafe impl<'a, 'gc, 'cell, T: Rebind<'a>> Rebind<'a> for Vec<T> {
    type Output = Vec<T::Output>;
}

unsafe impl<'a, 'gc, 'cell, T: Rebind<'a>> Rebind<'a> for Box<T> {
    type Output = Box<T::Output>;
}

unsafe impl<'a, 'gc, 'cell, T: Rebind<'a>> Rebind<'a> for Option<T> {
    type Output = Option<T::Output>;
}

unsafe impl<'a, 'gc, 'cell, T: Rebind<'a>, R: Rebind<'a>> Rebind<'a> for Result<T, R> {
    type Output = Result<T::Output, R::Output>;
}

unsafe impl<'a, 'gc, 'cell, K: Rebind<'a>, V: Rebind<'a>> Rebind<'a>
    for std::collections::HashMap<K, V>
{
    type Output = std::collections::HashMap<K::Output, V::Output>;
}
