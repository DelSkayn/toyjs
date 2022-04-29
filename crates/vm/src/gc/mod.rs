mod ptr;
use std::{
    alloc::{self, Layout},
    cell::{Cell, UnsafeCell},
    marker::PhantomData,
    mem,
    ptr::{drop_in_place, NonNull},
};

use common::{cell_vec::CellVec, collections::HashMap};
pub use ptr::Gc;
use ptr::{Color, GcBoxPtr};

mod impl_trace;

#[macro_export]
macro_rules! root {
    ($arena:expr, $value:ident) => {
        let __guard = $arena._root($value);
        #[allow(unused_unsafe)]
        let $value = unsafe { $crate::gc::_rebind_to(&__guard, $value) };
    };
}

#[macro_export]
macro_rules! rebind {
    ($arena:expr, $value:expr) => {
        unsafe {
            // Detach from arena's lifetime.
            let ptr = Gc::from_ptr($value.into_ptr());
            // Ensure that the $arena is an arena.
            let a: &Arena = $arena;
            // Bind to the lifetime of the arena.
            $crate::gc::_rebind_to(a, ptr)
        }
    };
}

#[macro_export]
macro_rules! rebind_value {
    ($arena:expr, $value:expr) => {
        #[allow(unused_unsafe)]
        unsafe {
            let v: Value<'static, '_> = std::mem::transmute($value);
            // Ensure that the $arena is an arena.
            let a: &$crate::gc::Arena = $arena;
            // Bind to the lifetime of the arena.
            $crate::gc::_rebind_to(a, v)
        }
    };
}

#[cfg(test)]
mod test;

use crate::cell::{CellOwner, Id, LCell};

use self::ptr::GcBox;

/// Context struct for marking life pointers.
#[derive(Clone, Copy)]
pub struct Tracer<'a> {
    roots: &'a Roots,
}

impl<'a> Tracer<'a> {
    /// Mark a pointer as alive.
    pub fn mark<'cell, T: Trace + 'a>(self, gc: Gc<'a, 'cell, T>) {
        unsafe {
            // Pointer is valid as gc can only contain valid pointers.
            let r = gc.ptr.as_ref();
            if r.color.get() != Color::White {
                return;
            }

            r.color.set(Color::Gray);
            if T::needs_trace() {
                let ptr: GcBoxPtr<'a, 'cell> = gc.ptr;
                // Change the lifetimes to static.
                // Roots implementation ensures that the lifetime constraints are upheld.
                self.roots.grays.push(mem::transmute(ptr));
            }
        }
    }

    /// Mark a pointer with a dynamic type as alive.
    pub fn mark_dynamic<'cell>(self, gc: Gc<'a, 'cell, dyn Trace>) {
        unsafe {
            // Pointer is valid as gc can only contain valid pointers.
            let r = gc.ptr.as_ref();
            if r.color.get() != Color::White {
                return;
            }

            r.color.set(Color::Gray);
            // Change the lifetimes to static.
            // Roots implementation ensures that the lifetime constraints are upheld.
            self.roots.grays.push(mem::transmute(gc.ptr));
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
pub unsafe trait Trace {
    /// Returns whether the type can contain any Gc pointers and thus needs to be traced.
    ///
    /// The value returned is used as an optimization.
    /// Returning true when the type can not contain any Gc pointers is completely safe.
    fn needs_trace() -> bool
    where
        Self: Sized;

    /// Traces the type for any gc pointers.
    fn trace(&self, trace: Tracer);
}

/// # Safety
///
/// Implementor must ensure that the output only changes the lifetime which signifies the gc
/// lifetime.
pub unsafe trait Rebind<'a>: Sized {
    type Output;
}

#[doc(hidden)]
#[inline(always)] // this should compile down to nothing
pub unsafe fn _rebind_to<'rt, R, T>(_: &'rt R, v: T) -> T::Output
where
    T: Rebind<'rt>,
{
    _rebind(v)
}

#[doc(hidden)]
#[inline(always)] // this should compile down to nothing
pub unsafe fn _rebind<'rt, T>(v: T) -> T::Output
where
    T: Rebind<'rt>,
{
    use std::mem::ManuallyDrop;

    if mem::size_of::<T>() != mem::size_of::<T::Output>() {
        panic!(
            "type `{}` implements rebind but its `Output` is a different size. `{}` is {} bytes in size but `{}` is {} bytes",
            std::any::type_name::<T>(),
            std::any::type_name::<T>(),
            std::mem::size_of::<T>(),
            std::any::type_name::<T::Output>(),
            std::mem::size_of::<T::Output>(),
        );
    }

    union Transmute<T, U> {
        a: ManuallyDrop<T>,
        b: ManuallyDrop<U>,
    }

    ManuallyDrop::into_inner(
        (Transmute {
            a: ManuallyDrop::new(v),
        })
        .b,
    )
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Phase {
    Sleep,
    Wake,
    Mark,
    Sweep,
}

pub struct Roots {
    // All lifetimes are static as the implementation ensures that the values contained in the
    // pointer are never borrowed without a CellOwner and the values are valid as long as the
    // GcBoxPtr exists.
    /// The root values, point to alive values.
    roots: CellVec<GcBoxPtr<'static, 'static>>,

    owned_roots: UnsafeCell<HashMap<usize, GcBoxPtr<'static, 'static>>>,
    next_owned_id: Cell<usize>,

    /// Ptr's which have been marked as alive
    grays: CellVec<GcBoxPtr<'static, 'static>>,
    /// Ptr's which might have new alive values as the value pointed to has been mutated.
    grays_again: CellVec<GcBoxPtr<'static, 'static>>,

    /// Current pointer reached while cleaning up.
    sweep: Cell<Option<GcBoxPtr<'static, 'static>>>,
    sweep_prev: Cell<Option<GcBoxPtr<'static, 'static>>>,

    /// The list of all gc allocated pointers.
    all: Cell<Option<GcBoxPtr<'static, 'static>>>,

    total_allocated: Cell<usize>,
    remembered_size: Cell<usize>,
    wakeup_total: Cell<usize>,
    allocation_debt: Cell<f64>,

    phase: Cell<Phase>,
}

impl Drop for Roots {
    fn drop(&mut self) {
        unsafe {
            while let Some(x) = self.all.get() {
                self.all.set(x.as_ref().next.get());
                drop_in_place(x.as_ptr());
                let layout = Layout::for_value(x.as_ref());
                alloc::dealloc(x.as_ptr().cast(), layout);
            }
        }
    }
}

impl Roots {
    const PAUSE_FACTOR: f64 = 0.5;
    const TIMING_FACTOR: f64 = 1.5;
    const MIN_SLEEP: usize = 4096;

    pub fn new() -> Roots {
        Roots {
            roots: CellVec::new(),

            owned_roots: UnsafeCell::new(HashMap::default()),
            next_owned_id: Cell::new(0),

            grays: CellVec::new(),
            grays_again: CellVec::new(),

            sweep: Cell::new(None),
            sweep_prev: Cell::new(None),

            all: Cell::new(None),

            total_allocated: Cell::new(0),
            remembered_size: Cell::new(0),
            wakeup_total: Cell::new(Self::MIN_SLEEP),
            allocation_debt: Cell::new(0.0),

            phase: Cell::new(Phase::Sleep),
        }
    }

    fn write_barrier<'a, 'b, T: Trace + 'a>(&self, gc: Gc<'a, 'b, T>) {
        if !T::needs_trace() {
            return;
        }
        unsafe {
            if self.phase.get() == Phase::Mark && gc.ptr.as_ref().color.get() == Color::Black {
                gc.ptr.as_ref().color.set(Color::Gray);
                let ptr: GcBoxPtr<'a, 'b> = gc.ptr;
                self.grays_again.push(mem::transmute(ptr));
            }
        }
    }
}

#[doc(hidden)]
pub struct RootGuard<'rt>(pub &'rt Roots);

impl<'rt> Drop for RootGuard<'rt> {
    fn drop(&mut self) {
        self.0.roots.pop();
    }
}

thread_local! {
    static ARENA_ACTIVE: Cell<bool> = Cell::new(false);
}

pub struct Arena<'rt, 'cell> {
    roots: &'rt Roots,
    marker: Id<'cell>,
}

impl<'rt, 'cell> Drop for Arena<'rt, 'cell> {
    fn drop(&mut self) {
        ARENA_ACTIVE.with(|x| x.set(false));
    }
}

impl<'rt, 'cell> Arena<'rt, 'cell> {
    pub fn new(roots: &'rt Roots) -> Self {
        assert!(!ARENA_ACTIVE.with(|x| x.get()));

        ARENA_ACTIVE.with(|x| x.set(true));

        Arena {
            roots,
            marker: unsafe { Id::new() },
        }
    }

    #[doc(hidden)]
    pub fn _root<'gc, T: Trace + 'gc>(&self, t: Gc<'gc, 'cell, T>) -> RootGuard<'rt> {
        let ptr: GcBoxPtr = t.ptr;
        unsafe {
            self.roots.roots.push(mem::transmute(ptr));
        }
        RootGuard(self.roots)
    }

    #[doc(hidden)]
    pub unsafe fn _root_owned<'gc, T: Trace + 'gc>(
        &self,
        gc: Gc<'gc, 'cell, T>,
    ) -> (usize, &'rt Roots) {
        let id = self.roots.next_owned_id.get();
        self.roots.next_owned_id.set(id + 1);

        let p: GcBoxPtr = gc.into_ptr();
        (*self.roots.owned_roots.get()).insert(id, mem::transmute(p));

        (id, self.roots)
    }

    pub fn add<'gc, T>(&'gc self, v: T) -> Gc<'gc, 'cell, T::Output>
    where
        T: Rebind<'gc>,
        T::Output: Trace,
    {
        unsafe {
            let layout = Layout::new::<GcBox<T::Output>>();
            let ptr = NonNull::new(alloc::alloc(layout).cast::<GcBox<T::Output>>()).unwrap();
            ptr.as_ptr().write(GcBox {
                color: Cell::new(Color::White),
                next: Cell::new(self.roots.all.get()),
                value: LCell::new(_rebind(v)),
            });

            self.roots
                .total_allocated
                .set(self.roots.total_allocated.get() + layout.size());

            if self.roots.phase.get() == Phase::Sleep
                && self.roots.total_allocated.get() > self.roots.wakeup_total.get()
            {
                self.roots.phase.set(Phase::Wake);
            }

            if self.roots.phase.get() != Phase::Sleep {
                self.roots.allocation_debt.set(
                    self.roots.allocation_debt.get()
                        + layout.size() as f64
                        + layout.size() as f64 / Roots::TIMING_FACTOR,
                );
            }

            let dyn_ptr: GcBoxPtr = ptr;
            self.roots.all.set(Some(mem::transmute(dyn_ptr)));

            if self.roots.phase.get() == Phase::Sweep && self.roots.sweep_prev.get().is_none() {
                self.roots.sweep_prev.set(self.roots.all.get());
            }
            Gc {
                ptr,
                marker: PhantomData,
            }
        }
    }

    pub fn collect_full<'gc>(&'gc mut self, owner: &'gc CellOwner<'cell>) {
        self.roots.allocation_debt.set(f64::INFINITY);
        self.roots.phase.set(Phase::Wake);
        self.collect(owner);
    }

    pub fn collect<'gc>(&'gc mut self, owner: &'gc CellOwner<'cell>) {
        let roots = self.roots;

        unsafe {
            if roots.phase.get() == Phase::Sleep {
                return;
            }

            let work = roots.allocation_debt.get();
            let mut work_done = 0usize;

            //let mut tmp = HashSet::new();

            while work > work_done as f64 {
                match roots.phase.get() {
                    Phase::Wake => {
                        roots.sweep_prev.set(None);
                        // Trace the root, every value reachable by the root is live.
                        // All gc pointers will be put on into the grays array.
                        roots.roots.unsafe_iter().copied().for_each(|x| {
                            x.as_ref().color.set(Color::Gray);
                            roots.grays.push(mem::transmute::<_, NonNull<_>>(x));
                            work_done += mem::size_of_val(x.as_ref());
                        });
                        for x in (*roots.owned_roots.get()).values().copied() {
                            x.as_ref().color.set(Color::Gray);
                            roots.grays.push(mem::transmute::<_, NonNull<_>>(x));
                            work_done += mem::size_of_val(x.as_ref());
                        }

                        roots.phase.set(Phase::Mark);
                    }
                    Phase::Mark => {
                        if let Some(x) = roots.grays.pop() {
                            //assert!(tmp.insert(x.as_ptr()));
                            let size = mem::size_of_val(x.as_ref());
                            work_done += size;
                            let ptr: GcBoxPtr<'gc, 'cell> = mem::transmute(x);
                            ptr.as_ref().value.borrow(owner).trace(Tracer { roots });

                            x.as_ref().color.set(Color::Black);
                        } else if let Some(x) = roots.grays_again.pop() {
                            //assert!(!tmp.insert(x.as_ptr()));
                            let ptr: GcBoxPtr<'gc, 'cell> = mem::transmute(x);

                            ptr.as_ref().value.borrow(owner).trace(Tracer { roots });
                            x.as_ref().color.set(Color::Black);
                        } else {
                            roots.roots.for_each(|x| {
                                if x.as_ref().color.get() == Color::Gray {
                                    roots.grays.push(mem::transmute(x));
                                }
                            });
                            for x in (*roots.owned_roots.get()).values().copied() {
                                if x.as_ref().color.get() == Color::Gray {
                                    roots.grays.push(mem::transmute::<_, NonNull<_>>(x));
                                }
                            }

                            // Found new values in root
                            // Should continue tracing till no more free values are found.
                            if !roots.grays.is_empty() {
                                continue;
                            }

                            roots.phase.set(Phase::Sweep);
                            roots.sweep.set(roots.all.get());
                        }
                    }
                    Phase::Sweep => {
                        if let Some(x) = roots.sweep.get() {
                            roots.sweep.set(x.as_ref().next.get());
                            let layout = Layout::for_value(x.as_ref());

                            if x.as_ref().color.get() == Color::White {
                                if let Some(prev) = roots.sweep_prev.get() {
                                    prev.as_ref().next.set(x.as_ref().next.get());
                                } else {
                                    roots.all.set(x.as_ref().next.get());
                                }

                                roots
                                    .total_allocated
                                    .set(roots.total_allocated.get() - layout.size());
                                drop_in_place(x.as_ptr());
                                alloc::dealloc(x.as_ptr().cast(), layout);
                            } else {
                                roots
                                    .remembered_size
                                    .set(roots.remembered_size.get() + layout.size());
                                x.as_ref().color.set(Color::White);
                                roots.sweep_prev.set(Some(x));
                            }
                        } else {
                            roots.phase.set(Phase::Sleep);
                            roots.allocation_debt.set(0.0);
                            roots.wakeup_total.set(
                                roots.total_allocated.get()
                                    + ((roots.remembered_size.get() as f64 * Roots::PAUSE_FACTOR)
                                        .round()
                                        .min(usize::MAX as f64)
                                        as usize)
                                        .max(Roots::MIN_SLEEP),
                            );
                        }
                    }
                    Phase::Sleep => break,
                }
            }
            roots
                .allocation_debt
                .set((roots.allocation_debt.get() - work_done as f64).max(0.0));
        }
    }

    #[inline]
    pub fn write_barrier<'a, 'b, T: Trace + 'a>(&self, gc: Gc<'a, 'b, T>) {
        self.roots.write_barrier(gc);
    }
}
