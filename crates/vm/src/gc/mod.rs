use std::{
    alloc::{self, Layout},
    cell::Cell,
    marker::PhantomData,
    mem,
    ptr::{self, NonNull},
};

use common::cell_vec::CellVec;

use crate::cell::{CellOwner, LCell};

mod impl_trace;

#[derive(Clone, Copy, Eq, PartialEq)]
enum Color {
    White,
    Gray,
    Black,
}

/// A set of roots which mark the alive pointers in the GC.
pub struct RootSet<'cell> {
    roots: CellVec<GcBoxPtr<'static, 'cell>>,

    all: Cell<Option<GcBoxPtr<'static, 'cell>>>,
    grays: CellVec<GcBoxPtr<'static, 'cell>>,
    grays_again: CellVec<GcBoxPtr<'static, 'cell>>,

    sweep: Cell<Option<GcBoxPtr<'static, 'cell>>>,
    sweep_prev: Cell<Option<GcBoxPtr<'static, 'cell>>>,
    phase: Cell<Phase>,

    total_allocated: Cell<usize>,
    remembered_size: Cell<usize>,
    wakeup_total: Cell<usize>,
    allocation_debt: Cell<f64>,
}

impl<'cell> RootSet<'cell> {
    const PAUSE_FACTOR: f64 = 0.5;
    const TIMING_FACTOR: f64 = 1.5;
    const MIN_SLEEP: usize = 4096;

    pub fn new() -> Self {
        RootSet {
            all: Cell::new(None),
            roots: CellVec::new(),
            grays: CellVec::new(),
            grays_again: CellVec::new(),

            sweep: Cell::new(None),
            sweep_prev: Cell::new(None),
            phase: Cell::new(Phase::Sleep),

            total_allocated: Cell::new(0),
            remembered_size: Cell::new(0),
            wakeup_total: Cell::new(Self::MIN_SLEEP),
            allocation_debt: Cell::new(0.0),
        }
    }

    fn write_barrier<'rt, T: Trace<'rt, 'cell>>(&self, gc: Gc<'rt, 'cell, T>) {
        if !T::needs_trace() {
            return;
        }
        unsafe {
            if self.phase.get() == Phase::Mark && gc.ptr.as_ref().color.get() == Color::Black {
                gc.ptr.as_ref().color.set(Color::Gray);
                self.grays_again.push(mem::transmute(gc.ptr));
            }
        }
    }
}

impl Drop for RootSet<'_> {
    fn drop(&mut self) {
        unsafe {
            while let Some(x) = self.all.get() {
                self.all.set(x.as_ref().next.get());
                ptr::drop_in_place(x.as_ptr());
                let layout = Layout::for_value(x.as_ref());
                alloc::dealloc(x.as_ptr().cast(), layout);
            }
        }
    }
}

/// Context struct for marking life pointers.
#[derive(Clone, Copy)]
pub struct Tracer<'gc, 'cell>(pub(crate) &'gc RootSet<'cell>);

impl<'gc, 'cell> Tracer<'gc, 'cell> {
    /// Mark a pointer as alive.
    pub fn mark<T: Trace<'gc, 'cell> + 'gc>(self, gc: Gc<'gc, 'cell, T>) {
        unsafe {
            let r = gc.ptr.as_ref();
            if r.color.get() != Color::White {
                return;
            }

            r.color.set(Color::Gray);
            if T::needs_trace() {
                self.0.grays.push(mem::transmute(gc.ptr));
            }
        }
    }

    /// Mark a pointer with a dynamic type as alive.
    pub fn mark_dynamic(self, gc: Gc<'gc, 'cell, dyn Trace<'gc, 'cell>>) {
        unsafe {
            let r = gc.ptr.as_ref();
            if r.color.get() != Color::White {
                return;
            }

            r.color.set(Color::Gray);
            self.0.grays.push(mem::transmute(gc.ptr));
        }
    }
}

/// A trait for marking gc live pointers.
///
/// # Safety
///
/// This trait is very unsafe and will cause UB if not correctly implemented, as such, one should be very carefull to
/// implement this trait correctly.
///
/// The implementation must uphold the following guarentees.
///
/// - `needs_trace` returns true if the type to be implemented can contain `Gc` pointers.
/// - `trace` marks all pointers contained in the type and calls `trace` all types contained in this type which implement `Trace`.
/// - The type must not dereferences a `Gc` pointer in its drop implementation.
///
pub unsafe trait Trace<'gc, 'cell> {
    /// Returns whether the type can contain any Gc pointers and thus needs to be traced.
    ///
    /// The value returned is used as an optimization.
    /// Returning true when the type can not contain any Gc pointers is completely safe.
    fn needs_trace() -> bool
    where
        Self: Sized;

    /// Traces the type for any gc pointers.
    fn trace(&self, trace: Tracer<'gc, 'cell>);
}

struct GcBox<'cell, T: ?Sized> {
    next: Cell<Option<GcBoxPtr<'static, 'cell>>>,
    color: Cell<Color>,
    value: LCell<'cell, T>,
}

type GcBoxPtr<'gc, 'cell> = NonNull<GcBox<'cell, dyn Trace<'gc, 'cell>>>;

/// A pointer to a gc allocated value.
pub struct Gc<'gc, 'cell, T: ?Sized> {
    ptr: NonNull<GcBox<'cell, T>>,
    marker: PhantomData<&'gc ()>,
}

impl<'gc, 'cell, T: ?Sized> Copy for Gc<'gc, 'cell, T> {}
impl<'gc, 'cell, T: ?Sized> Clone for Gc<'gc, 'cell, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'gc, 'cell, T: ?Sized + Trace<'gc, 'cell> + 'static> Gc<'gc, 'cell, T> {
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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Phase {
    Sleep,
    Wake,
    Mark,
    Sweep,
}

/// A place which can allocate gc pointers.
pub struct Arena<'rt, 'cell> {
    root: &'rt RootSet<'cell>,
}

impl<'rt, 'cell> Arena<'rt, 'cell> {
    /// Create a new arena.
    pub fn new(root: &'rt RootSet<'cell>) -> Self {
        Arena { root }
    }

    /// Allocate a new garbage collected value.
    pub fn allocate<'gc, T: Trace<'gc, 'cell> + 'static>(&'gc self, t: T) -> Gc<'gc, 'cell, T> {
        unsafe {
            let layout = Layout::new::<GcBox<T>>();
            let ptr = NonNull::new(alloc::alloc(layout).cast::<GcBox<T>>()).unwrap();
            ptr.as_ptr().write(GcBox {
                color: Cell::new(Color::White),
                next: Cell::new(self.root.all.get()),
                value: LCell::new(t),
            });

            if self.root.phase.get() == Phase::Sleep
                && self.root.total_allocated.get() > self.root.wakeup_total.get()
            {
                self.root.phase.set(Phase::Wake);
            }

            if self.root.phase.get() != Phase::Sleep {
                self.root.allocation_debt.set(
                    self.root.allocation_debt.get()
                        + layout.size() as f64
                        + layout.size() as f64 / RootSet::TIMING_FACTOR,
                );
            }

            self.root.all.set(Some(mem::transmute(ptr)));

            if self.root.phase.get() == Phase::Sweep && self.root.sweep_prev.get().is_none() {
                self.root.sweep_prev.set(self.root.all.get());
            }
            Gc {
                ptr,
                marker: PhantomData,
            }
        }
    }

    pub fn root<'a, T: ?Sized>(&self, ptr: Gc<'a, 'cell, T>) -> GcRoot<'rt, 'cell, T> {
        let _res: GcRoot<'rt, 'cell, T> = unsafe { mem::transmute(ptr) };
        todo!()
    }

    /// Function must be called if the value behind a gc pointer could possible contain new gc
    /// pointers.
    ///
    #[inline]
    pub fn write_barrier<'gc, T: Trace<'gc, 'cell>>(&self, gc: Gc<'gc, 'cell, T>) {
        self.root.write_barrier(gc)
    }

    /// Run Collection to completion for a full cycle.
    pub fn collect_full<T: Trace<'rt, 'cell>>(&mut self, owner: &CellOwner<'cell>) {
        self.root.allocation_debt.set(f64::INFINITY);
        self.root.phase.set(Phase::Wake);
        self.collect(owner);
    }

    /// Run garbage collection if nessecery.
    pub fn collect(&mut self, owner: &CellOwner<'cell>) {
        let root = self.root;

        unsafe {
            if root.phase.get() == Phase::Sleep {
                return;
            }

            let work = root.allocation_debt.get();
            let mut work_done = 0usize;

            //let mut tmp = HashSet::new();

            while work > work_done as f64 {
                match root.phase.get() {
                    Phase::Wake => {
                        root.sweep_prev.set(None);
                        // Trace the root, every value reachable by the root is live.
                        // All gc pointers will be put on into the grays array.
                        root.roots.unsafe_iter().copied().for_each(|x| {
                            x.as_ref().color.set(Color::Gray);
                            root.grays.push(mem::transmute(x));
                            work_done += mem::size_of_val(x.as_ref());
                        });

                        root.phase.set(Phase::Mark);
                    }
                    Phase::Mark => {
                        if let Some(x) = root.grays.pop() {
                            //assert!(tmp.insert(x.as_ptr()));
                            let size = mem::size_of_val(x.as_ref());
                            work_done += size;
                            // ???
                            x.as_ref().value.borrow(owner).trace(Tracer(self.root));
                            x.as_ref().color.set(Color::Black);
                        } else if let Some(x) = root.grays_again.pop() {
                            //assert!(!tmp.insert(x.as_ptr()));
                            x.as_ref().value.borrow(owner).trace(Tracer(self.root));
                            x.as_ref().color.set(Color::Black);
                        } else {
                            root.roots.for_each(|x| {
                                if x.as_ref().color.get() == Color::Gray {
                                    root.grays.push(mem::transmute(x));
                                }
                            });

                            // Found new values in root
                            // Should continue tracing till no more free values are found.
                            if !root.grays.is_empty() {
                                continue;
                            }

                            root.phase.set(Phase::Sweep);
                            root.sweep.set(self.root.all.get());
                        }
                    }
                    Phase::Sweep => {
                        if let Some(x) = root.sweep.get() {
                            root.sweep.set(x.as_ref().next.get());
                            let size = mem::size_of_val(x.as_ref());

                            if x.as_ref().color.get() == Color::White {
                                if let Some(prev) = root.sweep_prev.get() {
                                    prev.as_ref().next.set(x.as_ref().next.get());
                                } else {
                                    root.all.set(x.as_ref().next.get());
                                }

                                root.total_allocated.set(root.total_allocated.get() - size);
                                ptr::drop_in_place(x.as_ptr());
                                let layout = Layout::for_value(x.as_ref());
                                alloc::dealloc(x.as_ptr().cast(), layout);
                            } else {
                                root.remembered_size.set(root.remembered_size.get() + size);
                                x.as_ref().color.set(Color::White);
                                root.sweep_prev.set(Some(x));
                            }
                        } else {
                            root.phase.set(Phase::Sleep);
                            root.allocation_debt.set(0.0);
                            root.wakeup_total.set(
                                root.total_allocated.get()
                                    + ((root.remembered_size.get() as f64 * RootSet::PAUSE_FACTOR)
                                        .round()
                                        .min(usize::MAX as f64)
                                        as usize)
                                        .max(RootSet::MIN_SLEEP),
                            );
                        }
                    }
                    Phase::Sleep => break,
                }
            }
            root.allocation_debt
                .set((root.allocation_debt.get() - work_done as f64).max(0.0));
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn gc() {
        crate::new_cell_owner!(owner);
        let root = RootSet::new();
        let arena = Arena::new(&root);

        let ptr = arena.allocate(1);
        *ptr.borrow_mut_untraced(&mut owner) += 1;

        assert_eq!(*ptr.borrow(&owner), 2);
    }
}
