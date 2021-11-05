use super::{GcArena, Trace};
use std::{
    cell::{Cell, UnsafeCell},
    fmt,
    marker::PhantomData,
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

    pub fn ptr_eq(self, other: Self) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

impl<T: Trace> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &(*self.0.as_ref().value.get()) }
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
        ctx.mark(*self)
    }
}

pub type RootValuePtr = NonNull<RootValue>;

pub struct RootValue {
    pub next: Cell<Option<RootValuePtr>>,
    pub prev: Cell<Option<RootValuePtr>>,
    pub ptr: GcBoxPtr,
}

pub struct GcRoot<T: Trace + 'static>(pub(crate) RootValuePtr, pub(crate) PhantomData<T>);

impl<T: Trace + 'static> Drop for GcRoot<T> {
    fn drop(&mut self) {
        unsafe {
            let val = self.0.as_ref();
            if let Some(next) = val.next.get() {
                next.as_ref().prev.set(val.prev.get());
            }
            if let Some(prev) = val.prev.get() {
                prev.as_ref().next.set(val.next.get());
            }
            Box::from_raw(self.0.as_ptr());
        }
    }
}

impl<T: Trace + 'static> Deref for GcRoot<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { &*(self.0.as_ref().ptr.as_ref().value.get() as *mut T) }
    }
}
