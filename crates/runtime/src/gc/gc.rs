use super::{GcArena, Trace};
use std::{
    cell::{Cell, UnsafeCell},
    fmt,
    ops::Deref,
    ptr::NonNull,
};

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum Color {
    White,
    Gray,
    Black,
}

pub struct GcBox<T: Trace + ?Sized> {
    pub(crate) color: Cell<Color>,
    pub(crate) next: Cell<Option<NonNull<GcBox<dyn Trace>>>>,
    pub(crate) value: UnsafeCell<T>,
}

pub struct Gc<T: Trace>(pub(crate) NonNull<GcBox<T>>);

impl<T: Trace> Copy for Gc<T> {}
impl<T: Trace> Clone for Gc<T> {
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
        this.0.as_ptr() as *mut _
    }

    #[inline]
    pub fn from_raw(this: *mut ()) -> Self {
        Gc(NonNull::new(this as *mut GcBox<T>).unwrap())
    }

    #[inline]
    pub fn get_ptr(self, context: &GcArena) -> *mut T {
        unsafe {
            context.write_barrier(self);
            self.0.as_ref().value.get()
        }
    }
}

impl<T: Trace> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &(*self.0.as_ref().value.get()) }
    }
}
