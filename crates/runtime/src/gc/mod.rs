/// A garbage collector based the gc-arena crate.
use std::{
    cell::{Cell, UnsafeCell},
    fmt, mem,
    ops::Deref,
    ptr::NonNull,
};

mod trace;
pub use trace::Trace;

mod gc;
pub use gc::{Color, Gc, GcBox};

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
                (*self.0.grays.get()).push(gc.0);
            }
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

pub struct GcArena {
    all: Cell<Option<NonNull<GcBox<dyn Trace>>>>,
    grays: UnsafeCell<Vec<NonNull<GcBox<dyn Trace>>>>,
    grays_again: UnsafeCell<Vec<NonNull<GcBox<dyn Trace>>>>,

    sweep: Cell<Option<NonNull<GcBox<dyn Trace>>>>,
    sweep_prev: Cell<Option<NonNull<GcBox<dyn Trace>>>>,
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
            grays: UnsafeCell::new(Vec::new()),
            grays_again: UnsafeCell::new(Vec::new()),

            sweep: Cell::new(None),
            sweep_prev: Cell::new(None),
            phase: Cell::new(Phase::Sleep),

            total_allocated: Cell::new(0),
            remembered_size: Cell::new(0),
            wakeup_total: Cell::new(MIN_SLEEP),
            allocation_debt: Cell::new(0.0),
        }
    }

    pub unsafe fn allocate<T: Trace + 'static>(&self, value: T) -> Gc<T> {
        let size = mem::size_of::<GcBox<T>>();
        self.total_allocated.set(self.total_allocated.get() + size);

        let ptr = NonNull::new_unchecked(Box::into_raw(Box::new(GcBox {
            color: Cell::new(Color::White),
            next: Cell::new(self.all.get()),
            value: UnsafeCell::new(value),
        })));

        if self.phase.get() == Phase::Sleep && self.total_allocated.get() > self.wakeup_total.get()
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

    pub unsafe fn write_barrier<T: Trace + 'static>(&self, gc: Gc<T>) {
        if self.phase.get() == Phase::Mark && gc.0.as_ref().color.get() == Color::Black {
            gc.0.as_ref().color.set(Color::Gray);
            (*self.grays_again.get()).push(gc.0);
        }
    }

    pub unsafe fn collect_all<T: Trace>(&self, root: &T) {
        self.allocation_debt.set(f64::INFINITY);
        self.phase.set(Phase::Wake);
        self.collect_debt(root);
    }

    pub unsafe fn collect_debt<T: Trace>(&self, root: &T) {
        let work = self.allocation_debt.get();
        let mut work_done = 0usize;

        while work > work_done as f64 {
            match self.phase.get() {
                Phase::Wake => {
                    root.trace(Ctx(self));
                    work_done += mem::size_of::<T>();
                    self.phase.set(Phase::Mark);
                }
                Phase::Mark => {
                    if let Some(x) = (*self.grays.get()).pop() {
                        let size = mem::size_of_val(x.as_ref());
                        work_done += size;
                        x.as_ref().color.set(Color::Black);
                    } else if let Some(x) = (*self.grays_again.get()).pop() {
                        (*x.as_ref().value.get()).trace(Ctx(self));
                        x.as_ref().color.set(Color::Black);
                    } else {
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
                            work_done += size;
                            self.total_allocated.set(self.total_allocated.get() - size);
                            Box::from_raw(x.as_ptr());
                        } else {
                            self.remembered_size.set(self.remembered_size.get() + size);
                            x.as_ref().color.set(Color::White);
                            self.sweep_prev.set(Some(x));
                        }
                    } else {
                        self.sweep_prev.set(None);
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
}
