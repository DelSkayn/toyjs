use std::{cell::Cell, marker::PhantomData, ptr::NonNull};

use crate::cell::{CellOwner, LCell};

use super::{Arena, Trace};

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

pub type GcBoxPtr<'gc, 'cell> = NonNull<GcBox<'cell, dyn Trace<'cell> + 'gc>>;

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

impl<'gc, 'cell, T: ?Sized + Trace<'cell> + 'gc> Gc<'gc, 'cell, T> {
    pub fn into_ptr(self) -> NonNull<GcBox<'cell, T>> {
        self.ptr
    }

    pub unsafe fn from_ptr(ptr: NonNull<GcBox<'cell, T>>) -> Self {
        Gc {
            ptr,
            marker: PhantomData,
        }
    }

    // Borrow the contained value
    #[inline]
    pub fn borrow<'a>(&'a self, owner: &'a CellOwner<'cell>) -> &'a T {
        unsafe { owner.borrow(&self.ptr.as_ref().value) }
    }
}

impl<'gc, 'cell, T: Trace<'cell> + 'gc> Gc<'gc, 'cell, T> {
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

    // Borrow the contained value mutably without requiring access to the arena,
    // Can be used with values which themselfs do not contain `Gc` pointers.
    //
    // # Panic
    //
    // Will panic if `T::needs_trace()` returns true.
    #[inline]
    pub unsafe fn borrow_mut_untraced_unchecked<'a, 'rt>(
        &'a self,
        owner: &'a mut CellOwner<'cell>,
    ) -> &'a mut T {
        owner.borrow_mut(&self.ptr.as_ref().value)
    }
}
