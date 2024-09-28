use std::{
    array,
    cell::Cell,
    marker::PhantomData,
    mem::{self, offset_of, MaybeUninit},
    pin::Pin,
    usize,
};

use common::{map_ptr, ptr::Raw};
use rustix::mm::ProtFlags;

use super::{
    cell::GcCell,
    list::{List, ListNode},
    mmap,
    roots::OwnedRoot,
    segment::{self, NurserySegment, RetainedSegment, SegmentHeader, SEGMENT_SIZE},
    vtable::Trace,
};
use crate::gc::cell::ForwardVTable;

pub const ALLOC_ALLIGNMENT: usize = 8;
pub const MIN_ALLOC_SIZE: usize = std::mem::size_of::<GcCell<()>>();
pub const MAX_ALLOC_SIZE: usize = segment::MAX_ALLOCATION.next_power_of_two() >> 1;
pub const NUM_BUCKETS: usize = MAX_ALLOC_SIZE.ilog2() as usize - MIN_ALLOC_SIZE.ilog2() as usize;

mod retained;
mod young;

pub enum Error {
    OutOfMemory,
}

pub enum TracingState {
    None,
    TracingYoung,
    TracingFull,
}

pub trait RootState: Sealed {}
trait Sealed {}

pub struct Free<'a> {
    marker: PhantomData<&'a mut Arena>,
}
impl<'a> Sealed for Free<'a> {}
impl<'a> RootState for Free<'a> {}

pub struct Rooted(());
impl Sealed for Rooted {}
impl RootState for Rooted {}

struct GcPtr<T>(Cell<Option<Raw<T>>>);
struct SomeGcPtr<T>(Cell<Raw<T>>);

impl<T> SomeGcPtr<T> {
    pub fn as_gc_ptr_ref(&self) -> &GcPtr<T> {
        assert!(const { mem::size_of::<GcPtr<T>>() == mem::size_of::<SomeGcPtr<T>>() });
        // SAFETY: GcPtr is an Option<Raw<T>> at it's core. Raw<T> implements null pointer
        // optimization. This means that SomeGcPtr is a strict subset of GcPtr as it has all the
        // bit patterns except for all 0
        unsafe { mem::transmute(self) }
    }
}

impl<T> GcPtr<T> {
    pub fn erase_ref(&self) -> &GcPtr<()> {
        assert!(const { mem::size_of::<GcPtr<T>>() == mem::size_of::<SomeGcPtr<T>>() });
        // SAFETY: T is strictly a marker type in T so GcPtr<T> and GcPtr<I> have the same Layout.
        unsafe { mem::transmute(self) }
    }
}

#[repr(transparent)]
pub struct Gc<R: RootState, V> {
    ptr: SomeGcPtr<V>,
    root: PhantomData<R>,
}

#[repr(transparent)]
pub struct OptionGc<R, V> {
    ptr: GcPtr<V>,
    root: PhantomData<R>,
}

#[repr(transparent)]
pub struct Marker(Arena);

impl Marker {
    pub unsafe fn wrap_arena(arena: &Arena) -> &Marker {
        // Safety: marker is a transparent wrapper over arena so this is safe.
        unsafe { mem::transmute(arena) }
    }

    pub fn mark<R: RootState, V>(&self, ptr: &Gc<R, V>) -> Result<(), Error> {
        unsafe { self.0.mark(ptr.ptr.as_gc_ptr_ref().erase_ref()) }
    }

    pub fn mark_option<R: RootState, V>(&self, ptr: &OptionGc<R, V>) -> Result<(), Error> {
        unsafe { self.0.mark(ptr.ptr.erase_ref()) }
    }
}

struct NurseryFinalizer {
    ptr: Raw<()>,
    callback: unsafe fn(Raw<GcCell<()>>),
}

pub struct Arena {
    // We keep one segment around so that we don't have to always allocated 2 segments.
    unused: Cell<Option<Raw<SegmentHeader>>>,

    young_segment: Option<Raw<NurserySegment>>,
    young_finalizers: Vec<Raw<GcCell<()>>>,

    // The normal retained list
    old_segments: List<RetainedSegment>,

    buckets: [List<ListNode>; NUM_BUCKETS],

    owned_roots: List<OwnedRoot<()>>,

    state: TracingState,

    pending: Cell<Option<Raw<GcCell<()>>>>,
}

impl Arena {
    pub unsafe fn new() -> Self {
        Arena {
            unused: Cell::new(None),
            young_segment: None,
            young_finalizers: Vec::new(),
            old_segments: List::new(),
            buckets: array::from_fn(|_| List::new()),
            owned_roots: List::new(),
            state: TracingState::None,
            pending: Cell::new(None),
        }
    }

    pub fn root_owned<T: Trace>(self: Pin<&Self>, root: Pin<&OwnedRoot<T::Rooted>>, value: T) {
        let this = self.get_ref();
        unsafe {
            root.value
                .get()
                .write(MaybeUninit::new(value.assert_rooted()))
        }
        this.owned_roots.push_back(OwnedRoot::erase_pin_type(root))
    }

    pub fn alloc<'a, T: Trace>(
        self: Pin<&'a mut Self>,
        value: T,
    ) -> Result<Gc<Free<'a>, T>, Error> {
        let ptr = unsafe { self.get_unchecked_mut().alloc_raw(value)? };
        Ok(Gc {
            ptr: SomeGcPtr(Cell::new(ptr)),
            root: PhantomData,
        })
    }

    unsafe fn alloc_raw<T: Trace>(&mut self, value: T) -> Result<Raw<T>, Error> {
        assert!(
            const { mem::align_of::<T>() == ALLOC_ALLIGNMENT },
            "All values allocated in the gc need to have a {ALLOC_ALLIGNMENT} byte allignment"
        );
        // This should hold because above, but just to make sure.
        assert!(const { mem::align_of::<GcCell<T>>() == 8 },);
        assert!(
            const { mem::size_of::<T>() <= segment::MAX_ALLOCATION },
            "Allocation exceeded maximum allowed value"
        );
        assert!(
            const { mem::size_of::<T>() + mem::size_of::<GcCell<T>>() == mem::size_of::<GcCell<T>>() },
            "Spacing within cell"
        );

        let (ptr, alloced_young) = self.alloc_raw_sized(mem::size_of::<T>())?;
        let ptr = ptr.cast::<GcCell<T>>();
        ptr.write(GcCell::new(value));
        if mem::needs_drop::<T>() && alloced_young {
            self.young_finalizers.push(ptr.cast());
        }

        Ok(ptr.map_ptr(map_ptr!(GcCell::<T>, value)).cast())
    }

    unsafe fn alloc_raw_sized(&mut self, size: usize) -> Result<(Raw<u8>, bool), Error> {
        if let Some(ptr) = self.alloc_young(size) {
            return Ok((ptr, true));
        }
        // young generation is full, do a young collection cycle

        // Return error?
        self.collect_young();

        // at this point young should be completely free
        // if collection still fails just assume out of memory
        if let Some(ptr) = self.alloc_young(size) {
            return Ok((ptr, true));
        }

        Err(Error::OutOfMemory)
    }

    unsafe fn alloc_young(&mut self, size: usize) -> Option<Raw<u8>> {
        let mut nursery = unsafe { self.get_young_segment() };
        let offset = nursery.as_borrow().offset;
        let alloc_size = 
        if offset > (segment::SEGMENT_SIZE - size) as u32 {
            return None;
        }
        nursery.as_borrow_mut().offset += size as u32;
        Some(nursery.cast::<u8>().add(size))
    }

    unsafe fn alloc_old(&mut self, size: usize) -> Option<Raw<u8>> {}

    unsafe fn mark(&self, ptr: &GcPtr<()>) -> Result<(), Error> {
        let Some(unwrapped_ptr) = ptr.0.get() else {
            return Ok(());
        };

        let cell_ptr = unwrapped_ptr
            .cast::<u8>()
            .sub_bytes(offset_of!(GcCell<()>, value))
            .cast::<GcCell<()>>();

        let Some(v_table) = cell_ptr.as_borrow().forward_vtable.get_vtable() else {
            let forward_ptr = cell_ptr.as_borrow().forward_vtable.get_forward().unwrap();
            let value_ptr = forward_ptr
                .map_ptr(map_ptr!(GcCell<()>, value, value))
                .cast();
            ptr.0.set(Some(value_ptr.cast()));
            return Ok(());
        };

        // Safety: We checked above whether the pointer is not null.
        // So this pointer is within the allowed bit pattern of SomeGcPtr.
        let inhabited_ptr = mem::transmute::<&GcPtr<()>, &SomeGcPtr<()>>(ptr);

        match self.state {
            TracingState::TracingYoung => self.mark_young(v_table, inhabited_ptr),
            TracingState::TracingFull => self.mark_retained(v_table, ptr),
            _ => panic!("Marking in invalid tracing state"),
        }
    }

    unsafe fn forward_cell(
        &self,
        cell: Raw<GcCell<()>>,
        size: usize,
    ) -> Result<Raw<GcCell<()>>, Error> {
        debug_assert!(!cell.as_().forward_vtable.is_forwarded());

        if let Some(forward) = self.alloc_retained(size) {
            std::ptr::copy_nonoverlapping(
                cell.map_ptr(map_ptr!(GcCell, value)).cast::<u8>().as_ptr(),
                forward.as_ptr(),
                size,
            );

            let forward: Raw<GcCell<()>> = forward.cast();

            cell.map_ptr(map_ptr!(GcCell, forward_vtable))
                .write(ForwardVTable::from_forward(forward.cast()));

            cell.map_ptr(map_ptr!(GcCell, value))
                .cast::<Option<Raw<GcCell<()>>>>()
                .write(self.pending);

            self.pending = Some(cell);

            return Ok(());
        }

        self.collect_full();

        if let Some(forward) = self.alloc_retained(size) {
            std::ptr::copy_nonoverlapping(
                cell.map_ptr(map_ptr!(GcCell, value)).cast::<u8>().as_ptr(),
                forward.as_ptr(),
                size,
            );

            let forward: Raw<GcCell<()>> = forward.cast();

            cell.map_ptr(map_ptr!(GcCell, forward_vtable))
                .write(ForwardVTable::from_forward(forward.cast()));

            cell.map_ptr(map_ptr!(GcCell, value))
                .cast::<Option<Raw<GcCell<()>>>>()
                .write(self.pending);

            self.pending = Some(cell);

            return Ok(());
        }

        Err(())
    }

    #[cold]
    unsafe fn segment_allocate(&self) -> Raw<SegmentHeader> {
        if let Some(x) = self.unused.take() {
            return x;
        }
        match unsafe {
            mmap::mmap_aligned(SEGMENT_SIZE, ProtFlags::WRITE | ProtFlags::READ).unwrap()
        } {
            mmap::AlignedMmap::Single(x) => {
                let segment = unsafe { segment::init_segment(Raw::from_ptr(x).unwrap().cast()) };
                return segment;
            }
            mmap::AlignedMmap::Double(x) => {
                let mmap = Raw::from_ptr(x).unwrap();
                let segment_a = unsafe { segment::init_segment(mmap) };
                let segment_b = unsafe { segment::init_segment(mmap.add(SEGMENT_SIZE)) };
                self.unused.set(Some(segment_b));
                return segment_a;
            }
        }
    }

    unsafe fn segment_free(&self, segment: Raw<SegmentHeader>) {
        if self.unused.get().is_some() {
            mmap::munmap(segment.cast().as_ptr(), SEGMENT_SIZE).unwrap();
        } else {
            self.unused.set(Some(segment));
        }
    }

    #[cold]
    unsafe fn segment_nursery_allocate(&mut self) {
        let segment = self.segment_allocate();
        let segment = segment::init_nursery(segment);
        self.young_segment = Some(segment);
    }

    #[cold]
    unsafe fn segment_retained_allocate(&self) -> Raw<RetainedSegment> {
        let segment = self.segment_allocate();
        segment::init_retained(segment)
    }

    unsafe fn get_young_segment(&mut self) -> Raw<NurserySegment> {
        if let Some(x) = self.young_segment {
            x
        } else {
            self.segment_nursery_allocate();
            self.young_segment.unwrap()
        }
    }

    fn get_finalizer<T>() -> Option<unsafe fn(Raw<GcCell<usize>>)> {
        if std::mem::needs_drop::<T>() {
            Some(|ptr| unsafe {
                if !ptr.as_borrow().forward_vtable.is_forwarded() {
                    std::ptr::drop_in_place(ptr.as_ptr())
                }
            })
        } else {
            None
        }
    }

    unsafe fn alloc_size(&mut self, size: usize) -> Option<Raw<u8>> {
        if let Some(x) = self.alloc_young(size) {
            return Some(x);
        }

        todo!()
    }

    unsafe fn unmap_segment(segment: Raw<SegmentHeader>) {
        mmap::munmap(segment.cast().as_ptr(), SEGMENT_SIZE).unwrap();
    }
}
