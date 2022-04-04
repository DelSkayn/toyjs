use crate::atom::Atoms;

use super::Ctx;

/// Objects which can be contained in a gc pointer.
/// Must be implemented correctly or else will cause undefined behaviour.
///
/// # Safety
/// the function trace must both call trace on objects which require a trace
/// and mark all gc pointers directly contained by the structure.
pub unsafe trait Trace {
    /// Return wether this type of value needs a trace.
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    /// Trace all values.
    ///
    /// # Safety
    ///
    /// This function should trace and mark **all** traceble and `Gc` pointer values inside this
    /// object.
    fn trace(&self, ctx: Ctx);

    /// Returns the type name.
    /// Usefull for debugging gc bugs.
    fn name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }

    fn finalize(&self, _atoms: &Atoms) {}
}

macro_rules! impl_trace_primitive{
    ($($ty:ident,)*) => {
        $(
            unsafe impl Trace for $ty{
                fn needs_trace() -> bool{
                    false
                }

                fn trace(&self, _ctx: Ctx){}
            }
        )*
    };
}

impl_trace_primitive!(
    bool, char, u8, u16, u32, u64, usize, i8, i16, i32, i64, f32, f64, isize, String, str,
);

unsafe impl<T: Trace> Trace for &T {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    #[inline]
    fn trace(&self, ctx: Ctx) {
        (**self).trace(ctx);
    }
}

unsafe impl<T: Trace> Trace for &mut T {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    #[inline]
    fn trace(&self, ctx: Ctx) {
        (**self).trace(ctx);
    }
}

unsafe impl<T: Trace> Trace for Box<T> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    fn trace(&self, ctx: Ctx) {
        (**self).trace(ctx);
    }
}

unsafe impl<T: Trace> Trace for &[T] {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        T::needs_trace()
    }

    fn trace(&self, ctx: Ctx) {
        self.iter().for_each(|x| x.trace(ctx));
    }
}

unsafe impl<K: Trace, V: Trace, H> Trace for std::collections::HashMap<K, V, H> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        K::needs_trace() || V::needs_trace()
    }

    fn trace(&self, ctx: Ctx) {
        self.iter().for_each(|(k, v)| {
            if K::needs_trace() {
                k.trace(ctx);
            }
            if V::needs_trace() {
                v.trace(ctx);
            }
        });
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
        (**self).trace(ctx);
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

macro_rules! impl_trace_tuple{
    ($($ty:ident,)*) => {
            #[allow(non_snake_case)]
            unsafe impl<$($ty:Trace,)*> Trace for ($($ty,)*){
                fn needs_trace() -> bool{
                    false $(|| $ty::needs_trace())*
                }

                fn trace(&self, ctx: Ctx){
                    let ($(ref $ty,)*) = self;
                    $($ty.trace(ctx);)*
                }
            }
    };
}

impl_trace_tuple!(A,);
impl_trace_tuple!(A, B,);
impl_trace_tuple!(A, B, C,);
impl_trace_tuple!(A, B, C, D,);
impl_trace_tuple!(A, B, C, D, E,);
impl_trace_tuple!(A, B, C, D, E, F,);
impl_trace_tuple!(A, B, C, D, E, F, G,);
impl_trace_tuple!(A, B, C, D, E, F, G, H,);
