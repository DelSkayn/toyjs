use std::{cell::Cell, marker::PhantomData};

use common::ptr::{Raw, Ref};

use super::{segment, Arena};

pub trait RootState: Sealed {}
trait Sealed {}

/// Trait indicating that a GC pointer is unrooted.
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Free<'a> {
    marker: PhantomData<&'a mut Arena>,
}
impl<'a> Sealed for Free<'a> {}
impl<'a> RootState for Free<'a> {}

/// Trait indicating that a GC pointer is rooted.
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Rooted(());
impl Sealed for Rooted {}
impl RootState for Rooted {}

/// A wrapped gc pointer.
#[repr(transparent)]
pub struct Gc<R: RootState, T> {
    pointer: GcPtr<T>,
    _rooted: PhantomData<R>,
}

/// The inner gc pointer
type GcPtr<T> = Cell<Option<Raw<T>>>;

impl<R: RootState, T> Gc<R, T> {
    pub unsafe fn from_raw(ptr: Raw<T>) -> Self {
        debug_assert!(unsafe { segment::is_valid(ptr.cast()) });
        Self {
            pointer: Cell::new(Some(ptr)),
            _rooted: PhantomData,
        }
    }

    pub fn as_raw(this: Self) -> Raw<T> {
        unsafe { this.pointer.get().unwrap_unchecked() }
    }
    /// Unroots a gc pointer binding it to the arena, this prevents any call which can run
    /// collection.
    pub fn unroot<'a>(&self, arena: &'a Arena) -> Gc<Free<'a>, T> {
        let _ = arena;
        Gc {
            pointer: Cell::new(self.pointer.get()),
            _rooted: PhantomData,
        }
    }

    /// Update a pointer in place.
    pub fn update<O: RootState>(&self, other: Gc<O, T>) {
        unsafe {
            segment::write_barrier_retained_pointer(Ref::from(self).into_raw().cast());
        }

        self.pointer.set(other.pointer.get());
    }

    /// Borrow the pointer.
    ///
    /// Requires a borrow of the arena to enfore xor borrow model. Furthermore collection must not
    /// run while a value is borrowed to avoid a value being moved out from under a reference.
    pub fn borrow<'a>(&self, arena: &'a Arena) -> &'a T {
        let _ = arena;
        // Safety:
        // - Pointer is guarenteed to be valid as long as it is wrapped in this state.
        // - Gc cannot have a none inside of it so unwraping the pointer is always valid.
        unsafe {
            self.pointer
                .get()
                .unwrap_unchecked()
                .into_ref()
                .into_borrow()
        }
    }

    /// Borrow the pointer mutably.
    ///
    /// Requires a borrow of the arena to enfore xor borrow model. Furthermore collection must not
    /// run while a value is borrowed to avoid a value being moved out from under a reference.
    pub fn borrow_mut<'a>(&self, arena: &'a mut Arena) -> &'a mut T {
        unsafe {
            let _ = arena;
            // Safety:
            // - Gc cannot have a none inside of it so unwraping the pointer is always valid.
            let ptr = self.pointer.get().unwrap_unchecked();

            // Safety:
            // - Gc must always have a pointer pointing to a segment allocated value.
            segment::write_barrier_retained_pointer(ptr.cast());

            // Safety:
            // - Pointer is guarenteed to be valid as long as it is wrapped in this state.
            // - Write barrier ensures that new values added to target will be tracked properly.
            // - Gc cannot have a none inside of it so unwraping the pointer is always valid.
            ptr.into_mut().into_borrow_mut()
        }
    }
}
