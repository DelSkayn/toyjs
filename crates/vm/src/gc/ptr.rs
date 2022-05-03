use std::{cell::Cell, fmt, marker::PhantomData, ptr::NonNull};

use crate::cell::{CellOwner, LCell};

use super::{Arena, Rebind, Trace};

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum Color {
    White,
    Gray,
    Black,
}

/// A struct containing the Gc'd value.
pub struct GcBox<'cell, T: ?Sized> {
    pub next: Cell<Option<GcBoxPtr<'static, 'cell>>>,
    pub color: Cell<Color>,
    pub value: LCell<'cell, T>,
}

unsafe impl<'cell, T: Trace> Trace for GcBox<'cell, T> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, trace: super::Tracer) {
        if self.color.get() == Color::Gray {
            return;
        }
        self.color.set(Color::Gray);
        if T::needs_trace() {
            unsafe { (*self.value.get()).trace(trace) }
        }
    }
}

pub type GcBoxPtr<'gc, 'cell> = NonNull<GcBox<'cell, dyn Trace + 'gc>>;

/// A pointer to a gc allocated value.
pub struct Gc<'gc, 'cell, T: ?Sized> {
    pub(super) ptr: NonNull<GcBox<'cell, T>>,
    pub(super) marker: PhantomData<&'gc ()>,
}

impl<'gc, 'cell, T> fmt::Debug for Gc<'gc, 'cell, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Gc")
            .field("ptr", &self.ptr)
            .field("marker", &self.marker)
            .finish()
    }
}

impl<'gc, 'cell, T: ?Sized> Copy for Gc<'gc, 'cell, T> {}
impl<'gc, 'cell, T: ?Sized> Clone for Gc<'gc, 'cell, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'gc, 'cell, T: ?Sized + Trace + 'gc> Gc<'gc, 'cell, T> {
    pub fn into_ptr(self) -> NonNull<GcBox<'cell, T>> {
        self.ptr
    }

    /// # Safety
    ///
    /// The pointer given must be one obtained from `[Gc::into_ptr]`
    pub unsafe fn from_ptr(ptr: NonNull<GcBox<'cell, T>>) -> Self {
        Gc {
            ptr,
            marker: PhantomData,
        }
    }

    pub fn ptr_eq(self, other: Gc<'_, 'cell, T>) -> bool {
        std::ptr::eq(self.ptr.as_ptr(), other.ptr.as_ptr())
    }

    // Borrow the contained value
    #[inline(always)]
    pub fn borrow<'a>(self, owner: &'a CellOwner<'cell>) -> &'a T {
        unsafe { owner.borrow(&self.ptr.as_ref().value) }
    }

    pub fn get(self) -> *mut T {
        unsafe { &(*self.ptr.as_ptr()) }.value.get()
    }
}

unsafe impl<'a, 'gc, 'cell, T: Rebind<'a>> Rebind<'a> for Gc<'gc, 'cell, T> {
    type Output = Gc<'a, 'cell, <T as Rebind<'a>>::Output>;
}

impl<'a, 'gc, 'cell, A> Gc<'gc, 'cell, A>
where
    A: Rebind<'a> + Trace + 'a,
{
    // Borrow the contained value mutably
    #[inline]
    pub fn borrow_mut_2<'rt, B>(
        _owner: &'a mut CellOwner<'cell>,
        arena: &Arena<'_, 'cell>,
        a: Gc<'gc, 'cell, A>,
        b: Gc<'gc, 'cell, B>,
    ) -> (&'a mut A::Output, &'a mut B::Output)
    where
        B: Rebind<'a> + Trace + 'a,
    {
        assert_ne!(a.ptr.as_ptr() as usize, b.ptr.as_ptr() as usize);

        if A::needs_trace() {
            arena.write_barrier(a);
        }
        if B::needs_trace() {
            arena.write_barrier(b);
        }

        let a = unsafe { super::rebind(&mut (*a.get())) };
        let b = unsafe { super::rebind(&mut (*b.get())) };
        (a, b)
    }

    // Borrow the contained value mutably
    #[inline]
    pub fn borrow_mut_3<'rt, B, C>(
        _owner: &'a mut CellOwner<'cell>,
        arena: &Arena<'_, 'cell>,
        a: Gc<'gc, 'cell, A>,
        b: Gc<'gc, 'cell, B>,
        c: Gc<'gc, 'cell, C>,
    ) -> (&'a mut A::Output, &'a mut B::Output, &'a mut C::Output)
    where
        B: Rebind<'a> + Trace + 'a,
        C: Rebind<'a> + Trace + 'a,
    {
        assert_ne!(a.ptr.as_ptr() as usize, b.ptr.as_ptr() as usize);
        assert_ne!(a.ptr.as_ptr() as usize, c.ptr.as_ptr() as usize);
        assert_ne!(b.ptr.as_ptr() as usize, c.ptr.as_ptr() as usize);

        if A::needs_trace() {
            arena.write_barrier(a);
        }
        if B::needs_trace() {
            arena.write_barrier(b);
        }
        if C::needs_trace() {
            arena.write_barrier(c);
        }

        let a = unsafe { super::rebind(&mut (*a.get())) };
        let b = unsafe { super::rebind(&mut (*b.get())) };
        let c = unsafe { super::rebind(&mut (*c.get())) };
        (a, b, c)
    }
}

impl<'a, 'gc, 'cell, T> Gc<'gc, 'cell, T>
where
    T: Rebind<'a> + Trace + 'gc + 'a,
{
    // Borrow the contained value mutably
    #[inline]
    pub fn borrow_mut<'rt>(
        self,
        owner: &'a mut CellOwner<'cell>,
        arena: &Arena<'rt, 'cell>,
    ) -> &'a mut T::Output {
        if T::needs_trace() {
            arena.write_barrier(self);
        }
        unsafe { super::rebind(owner.borrow_mut(&self.ptr.as_ref().value)) }
    }

    /// Borrow the contained value mutably without requiring access to the arena,
    /// Can be used with values which themselfs do not contain `Gc` pointers.
    ///
    /// # Panic
    ///
    /// Will panic if `T::needs_trace()` returns true.
    #[inline]
    pub fn borrow_mut_untraced(self, owner: &'a mut CellOwner<'cell>) -> &'a mut T::Output {
        if T::needs_trace() {
            panic!("called borrow_mut_untraced on pointer which requires tracing")
        }
        unsafe { super::rebind(owner.borrow_mut(&self.ptr.as_ref().value)) }
    }

    /// Borrow the contained value mutably without requiring access to the arena,
    /// Can be used with values which themselfs do not contain `Gc` pointers.
    ///
    /// # Safety
    /// User should guarentee that no new gc pointers can be reached from this pointer after
    /// releasing the borrow.
    #[inline]
    pub unsafe fn unsafe_borrow_mut(self, owner: &'a mut CellOwner<'cell>) -> &'a mut T::Output {
        super::rebind(owner.borrow_mut(&self.ptr.as_ref().value))
    }
}
