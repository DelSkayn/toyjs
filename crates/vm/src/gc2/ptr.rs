use std::{cell::Cell, marker::PhantomData, mem, ptr::NonNull};

use crate::cell::{CellOwner, LCell};

use super::{Arena, RootGuard, Trace};

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Color {
    White,
    Gray,
    Black,
}

/// A struct containing the Gc'd value.
pub struct GcBox<'cell, T: ?Sized> {
    pub next: Cell<Option<GcBoxPtr<'static, 'static>>>,
    pub color: Cell<Color>,
    pub value: LCell<'cell, T>,
}

pub type GcBoxPtr<'gc, 'cell> = NonNull<GcBox<'cell, dyn Trace<'gc, 'cell> + 'gc>>;

/// A pointer to a gc allocated value.
pub struct Gc<'gc, 'cell, T: ?Sized> {
    pub(super) ptr: NonNull<GcBox<'cell, T>>,
    pub(super) marker: PhantomData<&'gc ()>,
}

impl<'gc, 'cell, T: ?Sized> Copy for Gc<'gc, 'cell, T> {}
impl<'gc, 'cell, T: ?Sized> Clone for Gc<'gc, 'cell, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'gc, 'cell, T: ?Sized + Trace<'gc, 'cell> + 'static> Gc<'gc, 'cell, T> {
    pub fn into_ptr(self) -> NonNull<GcBox<'cell, T>> {
        self.ptr
    }

    // Borrow the contained value
    #[inline]
    pub fn borrow<'a>(&'a self, owner: &'a CellOwner<'cell>) -> &'a T {
        unsafe { owner.borrow(&self.ptr.as_ref().value) }
    }
}

impl<'gc, 'cell, T: Trace<'gc, 'cell> + 'static> Gc<'gc, 'cell, T> {
    // Borrow the contained value mutably
    #[inline]
    pub fn borrow_mut<'a, 'rt>(
        &'a self,
        owner: &'a mut CellOwner<'cell>,
        arena: &Arena<'rt, 'cell>,
    ) -> &'a mut T {
        if T::needs_trace() {
            arena.write_barrier(*self);
        }
        unsafe { owner.borrow_mut(&self.ptr.as_ref().value) }
    }

    // Borrow the contained value mutably without requiring access to the arena,
    // Can be used with values which themselfs do not contain `Gc` pointers.
    //
    // # Panic
    //
    // Will panic if `T::needs_trace()` returns true.
    #[inline]
    pub fn borrow_mut_untraced<'a, 'rt>(&'a self, owner: &'a mut CellOwner<'cell>) -> &'a mut T {
        if T::needs_trace() {
            panic!("called borrow_mut_untraced on pointer which requires tracing")
        }
        unsafe { owner.borrow_mut(&self.ptr.as_ref().value) }
    }
}

/// A pointer to a gc allocated value which is rooted.
#[repr(transparent)]
pub struct GcRoot<'rt, 'cell, T: ?Sized> {
    ptr: Gc<'rt, 'cell, T>,
}

impl<'rt, 'cell, T: ?Sized> GcRoot<'rt, 'cell, T> {
    pub unsafe fn assume_rooted<'gc>(ptr: Gc<'gc, 'cell, T>) -> Self {
        Self {
            ptr: mem::transmute(ptr),
        }
    }

    #[doc(hidden)]
    pub unsafe fn assume_rooted_guard<'gc>(
        _guard: &'rt RootGuard<'_>,
        ptr: Gc<'gc, 'cell, T>,
    ) -> GcRoot<'rt, 'cell, T> {
        Self {
            ptr: mem::transmute(ptr),
        }
    }
}

impl<'gc, 'cell, T: ?Sized> Copy for GcRoot<'gc, 'cell, T> {}
impl<'gc, 'cell, T: ?Sized> Clone for GcRoot<'gc, 'cell, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'rt, 'cell, T: ?Sized> std::ops::Deref for GcRoot<'rt, 'cell, T> {
    type Target = Gc<'rt, 'cell, T>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.ptr
    }
}
