//! A garbage collector based on the gc-arena crate.
use std::{
    cell::{Cell, UnsafeCell},
    mem,
    ptr::NonNull,
};
mod trace;
use common::cell_vec::CellVec;
pub use trace::Trace;

mod ptr;
pub use ptr::Gc;
use ptr::{Color, GcBox, GcBoxPtr};

const PAUSE_FACTOR: f64 = 0.5;
const TIMING_FACTOR: f64 = 1.5;
const MIN_SLEEP: usize = 4096;

#[derive(Clone, Copy)]
pub struct Ctx<'gc>(&'gc GcArena);

impl<'gc> Ctx<'gc> {
    #[inline]
    pub fn mark<T: Trace + 'static>(self, gc: Gc<T>) {
        unsafe {
            gc.0.as_ref().color.set(Color::Gray);
            if T::needs_trace() {
                self.0.grays.push(gc.0);
            }
        }
    }

    pub fn mark_dynamic(self, gc: Gc<dyn Trace>) {
        unsafe {
            gc.0.as_ref().color.set(Color::Gray);
            self.0.grays.push(gc.0)
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Phase {
    Sleep,
    Wake,
    Mark,
    Sweep,
}

/// A structure for creating pointers managed by a garbage collector.
/// Allocating and using the pointers provide by the Gc are all save,
/// However it is unsafe to actually collect garbage because if the
/// colletion methods are not called properly the gc will free values which are still in use.
///
/// As such GcArena does not deallocate any pointers it allocated when it is dropped so one should
/// manually call `collect_all`.
pub struct GcArena {
    all: Cell<Option<GcBoxPtr>>,
    grays: CellVec<GcBoxPtr>,
    grays_again: CellVec<GcBoxPtr>,

    sweep: Cell<Option<GcBoxPtr>>,
    sweep_prev: Cell<Option<GcBoxPtr>>,
    phase: Cell<Phase>,

    total_allocated: Cell<usize>,
    remembered_size: Cell<usize>,
    wakeup_total: Cell<usize>,
    allocation_debt: Cell<f64>,
}

impl GcArena {
    pub fn new() -> Self {
        GcArena {
            all: Cell::new(None),
            grays: CellVec::new(),
            grays_again: CellVec::new(),

            sweep: Cell::new(None),
            sweep_prev: Cell::new(None),
            phase: Cell::new(Phase::Sleep),

            total_allocated: Cell::new(0),
            remembered_size: Cell::new(0),
            wakeup_total: Cell::new(MIN_SLEEP),
            allocation_debt: Cell::new(0.0),
        }
    }

    pub fn allocate<T: Trace + 'static>(&self, value: T) -> Gc<T> {
        unsafe {
            let size = mem::size_of::<GcBox<T>>();
            self.total_allocated.set(self.total_allocated.get() + size);

            let ptr = NonNull::new_unchecked(Box::into_raw(Box::new(GcBox {
                color: Cell::new(Color::White),
                next: Cell::new(self.all.get()),
                value: UnsafeCell::new(value),
            })));

            if self.phase.get() == Phase::Sleep
                && self.total_allocated.get() > self.wakeup_total.get()
            {
                self.phase.set(Phase::Wake);
            }

            if self.phase.get() != Phase::Sleep {
                self.allocation_debt
                    .set(self.allocation_debt.get() + size as f64 + size as f64 / TIMING_FACTOR)
            }

            self.all.set(Some(ptr));

            if self.phase.get() == Phase::Sweep && self.sweep_prev.get().is_none() {
                self.sweep_prev.set(self.all.get());
            }

            Gc(ptr)
        }
    }

    /// Function must be called if the value behind a gc pointer could possible contain new gc
    /// pointers.
    pub fn write_barrier<T: Trace + 'static>(&self, gc: Gc<T>) {
        if !T::needs_trace() {
            return;
        }
        unsafe {
            if self.phase.get() == Phase::Mark && gc.0.as_ref().color.get() == Color::Black {
                gc.0.as_ref().color.set(Color::Gray);
                self.grays_again.push(gc.0);
            }
        }
    }

    /// Run Collection to completion for a full cycle.
    /// Might not free all objects not reachable by root if they where reachable in a previous call
    /// if the gc is already running collection.
    ///
    /// # Safety
    /// All gc pointers held by the programm allocated with the current GcArena must be reachable
    /// from the root handed as an argument to this function.
    /// Any gc pointers which where not reachable from the program will be freed.
    pub unsafe fn collect_full<T: Trace>(&self, root: &T) {
        self.allocation_debt.set(f64::INFINITY);
        self.phase.set(Phase::Wake);
        self.collect_debt(root);
    }

    /// Run Collection for some time.
    /// Very unsafe, if not used correctly will cause undefined behaviour.
    ///
    /// # Safety
    /// All gc pointers held by the programm allocated with the current GcArena must be reachable
    /// from the root handed as an argument to this function.
    /// If any new pointers are added into a structure which could contain
    /// Any gc pointers which where not reachable from the program will be freed.
    pub unsafe fn collect_debt<T: Trace>(&self, root: &T) {
        let work = self.allocation_debt.get();
        let mut work_done = 0usize;

        while work > work_done as f64 {
            match self.phase.get() {
                Phase::Wake => {
                    self.sweep_prev.set(None);
                    // Trace the root, every value reachable by the root is live.
                    // All gc pointers will be put on into the grays array.
                    root.trace(Ctx(self));
                    work_done += mem::size_of::<T>();
                    self.phase.set(Phase::Mark);
                }
                Phase::Mark => {
                    if let Some(x) = self.grays.pop() {
                        let size = mem::size_of_val(x.as_ref());
                        work_done += size;
                        // ???
                        (*x.as_ref().value.get()).trace(Ctx(self));
                        x.as_ref().color.set(Color::Black);
                    } else if let Some(x) = self.grays_again.pop() {
                        (*x.as_ref().value.get()).trace(Ctx(self));
                        x.as_ref().color.set(Color::Black);
                    } else {
                        root.trace(Ctx(self));
                        self.grays.clear();
                        self.phase.set(Phase::Sweep);
                        self.sweep.set(self.all.get());
                    }
                }
                Phase::Sweep => {
                    if let Some(x) = self.sweep.get() {
                        self.sweep.set(x.as_ref().next.get());
                        let size = mem::size_of_val(x.as_ref());

                        if x.as_ref().color.get() == Color::White {
                            if let Some(prev) = self.sweep_prev.get() {
                                prev.as_ref().next.set(x.as_ref().next.get());
                            } else {
                                self.all.set(x.as_ref().next.get())
                            }
                            self.total_allocated.set(self.total_allocated.get() - size);
                            Box::from_raw(x.as_ptr());
                        } else {
                            self.remembered_size.set(self.remembered_size.get() + size);
                            x.as_ref().color.set(Color::White);
                            self.sweep_prev.set(Some(x));
                        }
                    } else {
                        self.phase.set(Phase::Sleep);
                        self.allocation_debt.set(0.0);
                        self.wakeup_total.set(
                            self.total_allocated.get()
                                + ((self.remembered_size.get() as f64 * PAUSE_FACTOR)
                                    .round()
                                    .min(usize::MAX as f64)
                                    as usize)
                                    .max(MIN_SLEEP),
                        );
                    }
                }
                Phase::Sleep => break,
            }
        }
        self.allocation_debt
            .set((self.allocation_debt.get() - work_done as f64).max(0.0));
    }

    pub unsafe fn collect_all(&self) {
        self.collect_full(&());
        self.collect_full(&());
        assert!(self.all.get().is_none());
    }
}
