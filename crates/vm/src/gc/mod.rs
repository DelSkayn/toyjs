mod ptr;
use std::{
    alloc::{self, Layout},
    cell::{Cell, UnsafeCell},
    marker::PhantomData,
    mem,
    ops::Deref,
    ptr::{drop_in_place, NonNull},
};

use common::{cell_vec::CellVec, collections::HashMap};
pub use ptr::Gc;
use ptr::{Color, GcBoxPtr};

mod impl_trace;

#[macro_export]
macro_rules! root {
    ($arena:expr, $value:ident) => {
        let $value = unsafe { $crate::gc::rebind($value) };
        let __guard = $arena._root_gc($value);
        #[allow(unused_unsafe)]
        let $value = unsafe { $crate::gc::rebind_to(&__guard, $value) };
    };
}

#[macro_export]
macro_rules! root_clone {
    ($arena:expr, $value:ident) => {
        let $value = unsafe { $crate::gc::rebind($value) };

        let __guard = $arena._root(&$value);
        #[allow(unused_unsafe)]
        let $value = unsafe { $crate::gc::rebind_to(&__guard, $value.clone()) };
    };
}

#[macro_export]
macro_rules! rebind {
    ($arena:expr, $value:expr) => {{
        let v = $value;
        unsafe {
            // Detach from arena's lifetime.
            let v = $crate::gc::rebind(v);
            // Ensure that the $arena is an arena.
            let a: &$crate::gc::Arena = $arena;
            // Bind to the lifetime of the arena.
            $crate::gc::rebind_to(a, v)
        }
    }};
}

#[macro_export]
macro_rules! rebind_try {
    ($arena:expr, $value:expr) => {
        match $value {
            Ok(x) => x,
            Err(e) => return Err($crate::rebind!($arena, e)),
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
    roots: &'a Roots<'static>,
}

impl<'a> Tracer<'a> {
    /// Mark a pointer as alive.
    pub fn mark<T: Trace>(self, gc: Gc<'_, '_, T>) {
        unsafe {
            // Pointer is valid as gc can only contain valid pointers.
            let r = gc.ptr.as_ref();
            if r.color.get() != Color::White {
                return;
            }

            r.color.set(Color::Gray);
            if T::needs_trace() {
                let ptr: GcBoxPtr<'_, '_> = gc.ptr;
                // Change the lifetimes to static.
                // Roots implementation ensures that the lifetime constraints are upheld.
                self.roots.grays.push(mem::transmute(ptr));
            }
        }
    }

    /// Mark a pointer with a dynamic type as alive.
    pub fn mark_dynamic(self, gc: Gc<'_, '_, dyn Trace>) {
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
/// - `needs_trace` returns true if the type can contain `Gc` pointers.
/// - `trace` marks all pointers contained in the type and calls `trace` all types contained in this type which implement `Trace`.
/// - The type must not dereferences a `Gc` pointer in its drop implementation.
///
pub unsafe trait Trace {
    /// Returns whether the type can contain any `Gc` pointers and needs to be traced.
    ///
    /// The value returned is used as an optimization.
    /// Returning true when a type cannot contain any `Gc` pointers is completely safe.
    fn needs_trace() -> bool
    where
        Self: Sized;

    /// Traces the type for any gc pointers.
    fn trace(&self, trace: Tracer);
}

/// Gc values who's lifetimes can be rebind
///
/// # Safety
///
/// Implementor must ensure that the output only changes the lifetime which signifies the gc
/// lifetime.
pub unsafe trait Rebind<'a> {
    type Output;
}

/// Rebind a value to the lifetime of a given borrow.
///
/// # Safety
///
/// See [`rebind()`]
#[inline(always)] // this should compile down to nothing
pub unsafe fn rebind_to<'rt, R, T>(_: &'rt R, v: T) -> T::Output
where
    T: Rebind<'rt>,
{
    rebind(v)
}

/// Rebinds a value to a arbitray lifetime.
///
/// # Safety
///
/// This method is wildly unsafe if not used correctly.
/// Rebinding is essentially a somewhat more restrictive [`std::mem::transmute`] and should be treated as
/// such.
///
/// In the context of the garbage collections rebind can be used to change the lifetime of a gc value
/// to some other gc value or to the lifetime of a borrowed arena.
///
/// Rebinding to a borrowed arena is always safe to do as holding onto an arena borrow prevents on
/// from running garbage collection. While possible to do with this function prefer the [`rebind!`]
/// macro as this one is guarteed to be safe.
///
/// The primary used for this macro is to change the lifetime of a gc'd value when inserting in a
/// traced collection.
/// DOCME: explain rebinding collections
///
#[inline(always)] // this should compile down to nothing
pub unsafe fn rebind<'rt, T>(v: T) -> T::Output
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

pub struct Roots<'cell> {
    // All lifetimes are static as the implementation ensures that the values contained in the
    // pointer are never borrowed without a CellOwner and the values are valid as long as the
    // GcBoxPtr exists.
    /// The root values, point to alive values.
    roots: CellVec<*const dyn Trace>,

    owned_roots: UnsafeCell<HashMap<usize, GcBoxPtr<'static, 'cell>>>,
    next_owned_id: Cell<usize>,

    /// Ptr's which have been marked as alive
    grays: CellVec<GcBoxPtr<'static, 'cell>>,
    /// Ptr's which might have new alive values as the value pointed to has been mutated.
    grays_again: CellVec<GcBoxPtr<'static, 'cell>>,

    /// Current pointer reached while cleaning up.
    sweep: Cell<Option<GcBoxPtr<'static, 'cell>>>,
    sweep_prev: Cell<Option<GcBoxPtr<'static, 'cell>>>,

    /// The list of all gc allocated pointers.
    all: Cell<Option<GcBoxPtr<'static, 'cell>>>,

    total_allocated: Cell<usize>,
    remembered_size: Cell<usize>,
    wakeup_total: Cell<usize>,
    allocation_debt: Cell<f64>,

    phase: Cell<Phase>,
}

impl<'cell> Drop for Roots<'cell> {
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

pub struct OwnedGc<'rt, 'cell, T: Trace> {
    roots: &'rt Roots<'cell>,
    ptr: Gc<'rt, 'cell, T>,
    id: usize,
}

impl<'rt, 'cell, T: Trace> Drop for OwnedGc<'rt, 'cell, T> {
    fn drop(&mut self) {
        unsafe {
            (*self.roots.owned_roots.get()).remove(&self.id);
        }
    }
}

impl<'rt, 'cell, T: Trace> Deref for OwnedGc<'rt, 'cell, T> {
    type Target = Gc<'rt, 'cell, T>;

    fn deref(&self) -> &Self::Target {
        &self.ptr
    }
}

pub struct RootFrame<'rt, 'cell> {
    roots: &'rt Roots<'cell>,
    len: usize,
}

impl<'rt, 'cell> Drop for RootFrame<'rt, 'cell> {
    fn drop(&mut self) {
        while self.roots.roots.len() > self.len {
            self.roots.roots.pop();
        }
    }
}

impl<'cell> Roots<'cell> {
    const PAUSE_FACTOR: f64 = 0.5;
    const TIMING_FACTOR: f64 = 1.5;
    const MIN_SLEEP: usize = 4096;

    pub fn new() -> Roots<'cell> {
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

    pub fn add_owned<'a, T>(&'a self, ptr: Gc<'_, 'cell, T>) -> OwnedGc<'a, 'cell, T::Output>
    where
        T: Rebind<'a> + Trace + 'a,
        T::Output: Trace + 'a,
    {
        let id = self.next_owned_id.get();
        self.next_owned_id.set(id + 1);
        unsafe {
            let dyn_ptr: GcBoxPtr = ptr.ptr;
            (*self.owned_roots.get()).insert(id, mem::transmute(dyn_ptr));
            OwnedGc {
                roots: self,
                ptr: rebind(ptr),
                id,
            }
        }
    }

    pub unsafe fn frame<'rt>(&'rt self) -> RootFrame<'rt, 'cell> {
        RootFrame {
            roots: self,
            len: self.roots.len(),
        }
    }

    pub unsafe fn push<T: Trace>(&self, ptr: Gc<'_, '_, T>) {
        let dyn_ptr: GcBoxPtr = ptr.ptr;
        self.roots.push(mem::transmute(dyn_ptr))
    }
}

#[doc(hidden)]
pub struct RootGuard<'rt, 'cell>(pub &'rt Roots<'cell>);

impl<'rt, 'cell> RootGuard<'rt, 'cell> {
    pub unsafe fn bind<'a, R: Rebind<'a>>(&'a self, r: R) -> R::Output {
        rebind(r)
    }
}

impl<'rt, 'cell> Drop for RootGuard<'rt, 'cell> {
    fn drop(&mut self) {
        self.0.roots.pop();
    }
}

thread_local! {
    static ARENA_ACTIVE: Cell<bool> = Cell::new(false);
}

pub struct Arena<'rt, 'cell> {
    roots: &'rt Roots<'cell>,
    marker: Id<'cell>,
}

impl<'rt, 'cell> Drop for Arena<'rt, 'cell> {
    fn drop(&mut self) {
        ARENA_ACTIVE.with(|x| x.set(false));
    }
}

impl<'rt, 'cell> Arena<'rt, 'cell> {
    pub fn new(roots: &'rt Roots<'cell>) -> Self {
        assert!(!ARENA_ACTIVE.with(|x| x.get()));

        ARENA_ACTIVE.with(|x| x.set(true));

        Arena {
            roots,
            marker: unsafe { Id::new() },
        }
    }

    pub unsafe fn new_unchecked(roots: &'rt Roots<'cell>) -> Self {
        Arena {
            roots,
            marker: Id::new(),
        }
    }

    pub fn roots(&self) -> &'rt Roots<'cell> {
        self.roots
    }

    #[doc(hidden)]
    pub fn _root_gc<T: Trace>(&self, t: Gc<'_, 'cell, T>) -> RootGuard<'rt, 'cell> {
        unsafe {
            let ptr: &GcBox<T> = t.ptr.as_ref();
            let ptr: &dyn Trace = ptr;
            self.roots.roots.push(mem::transmute(ptr));
        }
        RootGuard(self.roots)
    }

    #[doc(hidden)]
    pub fn _root<T: Trace>(&self, t: &T) -> RootGuard<'rt, 'cell> {
        unsafe {
            let ptr: &dyn Trace = t;
            self.roots.roots.push(mem::transmute(ptr));
        }
        RootGuard(self.roots)
    }

    #[doc(hidden)]
    pub unsafe fn _root_ptr<T: Trace>(&self, t: &mut T) -> RootGuard<'rt, 'cell> {
        let ptr: *const dyn Trace = t;
        self.roots.roots.push(mem::transmute(ptr));
        RootGuard(self.roots)
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
                value: LCell::new(rebind(v)),
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

    pub fn collect_full(&mut self, owner: &mut CellOwner<'cell>) {
        self.roots.allocation_debt.set(f64::INFINITY);
        self.roots.phase.set(Phase::Wake);
        self.collect(owner);
    }

    pub fn collect(&mut self, owner: &mut CellOwner<'cell>) {
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

                        roots.roots.unsafe_iter().copied().for_each(|x| {
                            (*x).trace(Tracer::<'rt> {
                                roots: mem::transmute(roots),
                            });
                            work_done += mem::size_of_val(&(*x));
                        });
                        for x in (*roots.owned_roots.get()).values().copied() {
                            x.as_ref().color.set(Color::Gray);
                            roots.grays.push(mem::transmute::<_, NonNull<_>>(x));
                            work_done += mem::size_of_val(x.as_ref());
                        }

                        roots.phase.set(Phase::Mark);
                    }
                    Phase::Mark => {
                        println!("mark");
                        if let Some(x) = roots.grays.pop() {
                            //assert!(tmp.insert(x.as_ptr()));
                            let size = mem::size_of_val(x.as_ref());
                            work_done += size;
                            let ptr: GcBoxPtr<'static, 'cell> = mem::transmute(x);
                            ptr.as_ref().value.borrow(owner).trace(Tracer::<'rt> {
                                roots: mem::transmute(roots),
                            });

                            x.as_ref().color.set(Color::Black);
                        } else if let Some(x) = roots.grays_again.pop() {
                            //assert!(!tmp.insert(x.as_ptr()));
                            let ptr: GcBoxPtr<'static, 'cell> = mem::transmute(x);

                            ptr.as_ref().value.borrow(owner).trace(Tracer::<'rt> {
                                roots: mem::transmute(roots),
                            });
                            x.as_ref().color.set(Color::Black);
                        } else {
                            let tracer = Tracer::<'rt> {
                                roots: mem::transmute(roots),
                            };
                            roots.roots.unsafe_iter().copied().for_each(|x| {
                                (*x).trace(tracer);
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
                                println!("free: {:?}", x.as_ptr());
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
    pub fn write_barrier<'a, T: Trace + 'a>(&self, gc: Gc<'a, 'cell, T>) {
        self.roots.write_barrier(gc);
    }
}
