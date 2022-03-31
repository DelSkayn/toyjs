use super::{GcArena, Trace};
use std::{
    cell::{Cell, UnsafeCell},
    fmt,
    ops::Deref,
    ptr::{addr_of, NonNull},
};

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum Color {
    White,
    Gray,
    Black,
}

pub struct GcBox<T: Trace + ?Sized> {
    #[cfg(feature = "dump-gc-trace")]
    pub free: Cell<bool>,
    pub(crate) color: Cell<Color>,
    pub(crate) next: Cell<Option<GcBoxPtr>>,
    pub(crate) value: UnsafeCell<T>,
}

pub struct Gc<T: Trace + ?Sized>(pub(crate) NonNull<GcBox<T>>);

pub type GcBoxPtr = NonNull<GcBox<dyn Trace>>;

impl<T: Trace + ?Sized> Copy for Gc<T> {}
impl<T: Trace + ?Sized> Clone for Gc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: fmt::Debug + Trace> fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Gc")
            .field("pointer", unsafe { &(*self.0.as_ref().value.get()) })
            .finish()
    }
}

impl<T: Trace + 'static> Gc<T> {
    #[inline]
    pub fn into_raw(this: Self) -> *mut () {
        this.0.as_ptr().cast::<()>()
    }

    #[inline]
    pub fn from_raw(this: *mut ()) -> Self {
        Gc(NonNull::new(this.cast::<GcBox<T>>()).unwrap())
    }

    #[inline]
    pub fn get_ptr(self, context: &GcArena) -> *mut T {
        unsafe {
            context.write_barrier(self);
            self.0.as_ref().value.get()
        }
    }

    #[inline]
    pub unsafe fn ref_static(&self) -> &'static T {
        &(*(*self.0.as_ptr()).value.get())
    }

    #[inline]
    pub fn ptr_eq(self, other: Self) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

impl<T: Trace> Deref for Gc<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe {
            let value = addr_of!((*self.0.as_ptr()).value);
            &(*(*value).get())
        }
    }
}

impl<T: PartialEq + Trace> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        (**self) == (**other)
    }
}

impl<T: Eq + Trace> Eq for Gc<T> {}

unsafe impl<T: Trace + 'static> Trace for Gc<T> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    #[inline]
    fn trace(&self, ctx: super::Ctx) {
        ctx.mark(*self);
    }
}
