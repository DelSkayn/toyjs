use std::{cell::Cell, marker::PhantomData, pin::Pin};

use common::ptr::{Mut, Raw, Ref};

use super::{segment, Arena, Trace};

#[allow(private_bounds)]
pub trait RootState: Sealed {}
#[allow(private_bounds)]
pub trait GcAlloced: Sealed {}
trait Sealed {}

/// Trait indicating that a GC pointer is unrooted.
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Free<'a> {
    marker: PhantomData<&'a mut Arena>,
}
impl<'a> Sealed for Free<'a> {}
impl<'a> RootState for Free<'a> {}
impl<'a> GcAlloced for Free<'a> {}

/// Trait indicating that a GC pointer is rooted.
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Rooted(());
impl Sealed for Rooted {}
impl RootState for Rooted {}
impl GcAlloced for OwnedRooted {}

/// Trait indicating that the GC pointer is rooted within an owned root
pub struct OwnedRooted(());
impl Sealed for OwnedRooted {}
impl RootState for OwnedRooted {}

/// A wrapped gc pointer.
///
#[repr(transparent)]
pub struct Gc<R: RootState, T> {
    /// The pointer here contains a Cell<Option<Raw<T>>>.
    /// This is to make this pointer equivalent in layout to [`OptionGc`] but this type may never
    /// contain `None` within the pointer value. Methods of this type assume that the value is not
    /// none.
    pointer: GcPtr<T>,
    _rooted: PhantomData<R>,
}

/// The inner gc pointer
type GcPtr<T> = Cell<Option<Raw<T>>>;

impl<R: RootState, T: Trace> Gc<R, T> {
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
    pub fn borrow_mut<'a>(&self, arena: &'a mut Arena) -> &'a mut T::Free<'a> {
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
            ptr.cast().into_mut().into_borrow_mut()
        }
    }

    /// Borrow the pointer mutably.
    ///
    /// Requires a borrow of the arena to enfore xor borrow model. Furthermore collection must not
    /// run while a value is borrowed to avoid a value being moved out from under a reference.
    ///
    /// This method is slightly faster then borrow_mut_pinned as it does not require a write
    /// barrier. However it returns a Pin<&mut T> instead of a normal T. This pinning should
    /// prevent any Gc pointer from being moved into or out of the value preventing invalidating
    /// gc pointers. Pointers might still be altered via the [`Gc::update`] methods which raise a write
    /// barrier themselves when called. Note that when updating multiple pointers on an object is
    /// might still be faster to call borrow_mut instead.
    pub fn borrow_mut_pinned<'a>(&self, arena: &'a mut Arena) -> Pin<&'a mut T> {
        unsafe {
            let _ = arena;
            // Safety:
            // - Gc cannot have a none inside of it so unwraping the pointer is always valid.
            let ptr = self.pointer.get().unwrap_unchecked();

            // Safety:
            // - Pointer is guarenteed to be valid as long as it is wrapped in this state.
            // - Write barrier ensures that new values added to target will be tracked properly.
            // - Gc cannot have a none inside of it so unwraping the pointer is always valid.
            Pin::new_unchecked(ptr.into_mut().into_borrow_mut())
        }
    }

    /// Checks two pointers for equality
    pub fn eq<OR: RootState>(this: Self, that: Gc<OR, T>) -> bool {
        unsafe {
            this.pointer.get().unwrap_unchecked().addr()
                == that.pointer.get().unwrap_unchecked().addr()
        }
    }
}

impl<R: RootState + GcAlloced, T: Trace> Gc<R, T> {
    /// Update a pointer in place.
    pub fn update<O: RootState>(&self, other: Gc<O, T>) {
        unsafe {
            segment::write_barrier_retained_pointer(Ref::from(self).into_raw().cast());
        }

        self.pointer.set(other.pointer.get());
    }
}

/// A gc pointer which might be None.
#[repr(transparent)]
pub struct OptionGc<R: RootState, T> {
    pointer: GcPtr<T>,
    _rooted: PhantomData<R>,
}

impl<R: RootState, T> OptionGc<R, T> {
    pub fn none() -> Self {
        OptionGc {
            pointer: Cell::new(None),
            _rooted: PhantomData,
        }
    }

    pub fn as_some(&self) -> Option<&Gc<R, T>> {
        if self.pointer.get().is_some() {
            unsafe { Some(Ref::from(self).cast().into_borrow()) }
        } else {
            None
        }
    }

    pub fn as_some_mut(&mut self) -> Option<&mut Gc<R, T>> {
        if self.pointer.get().is_some() {
            unsafe { Some(Mut::from(self).cast().into_borrow_mut()) }
        } else {
            None
        }
    }
}

impl<R: RootState + GcAlloced, T: Trace> OptionGc<R, T> {
    /// Update a pointer in place.
    pub fn update<O: RootState>(&self, other: OptionGc<O, T>) {
        unsafe {
            segment::write_barrier_retained_pointer(Ref::from(self).into_raw().cast());
        }

        self.pointer.set(other.pointer.get());
    }
}
