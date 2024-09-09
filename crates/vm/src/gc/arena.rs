use std::{alloc::Layout, mem, ptr, usize};

use common::{
    map_ptr,
    ptr::{Raw, Ref},
};
use rustix::mm::ProtFlags;

use super::{
    list::{List, ListNode},
    mmap,
    segment::{self, NurserySegment, RetainedSegment, SegmentHeader, SEGMENT_SIZE},
    vtable::VTable,
};

pub struct Gc<V>(Raw<V>);

pub struct Marker<'a> {
    arena: &'a Arena,
}

impl Marker<'_> {
    pub fn mark<V>(&self, ptr: Gc<V>) {}
}

pub unsafe trait Trace {
    const NAME: &str;

    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn v_table() -> &'static VTable
    where
        Self: Sized,
    {
        VTable::get::<Self>()
    }

    fn trace(&self, trace: &Marker);
}

union HeapHandle {
    free: u64,
    ptr: Raw<GcCell<()>>,
}

impl HeapHandle {
    pub fn handle(ptr: Raw<GcCell<()>>) -> Self {
        debug_assert!(ptr.addr().get() % 8 == 0, "Pointer not properly aligned");
        HeapHandle { ptr }
    }

    pub fn free(free: u32) -> Self {
        HeapHandle {
            free: ((free as u64) << 1) | 1,
        }
    }

    pub fn to_handle(self) -> Option<Raw<GcCell<()>>> {
        if unsafe { self.free & 1 == 0 } {
            unsafe { Some(self.ptr) }
        } else {
            None
        }
    }

    pub fn to_free(self) -> Option<u32> {
        if unsafe { self.free & 1 == 1 } {
            unsafe { Some((self.free >> 1) as u32) }
        } else {
            None
        }
    }
}

struct NuseryFinalizer {
    ptr: Raw<()>,
    callback: unsafe fn(Raw<()>),
}

#[derive(Clone, Copy)]
pub struct ForwardVTable(Raw<()>);

impl ForwardVTable {
    pub fn from_vtable(ptr: &'static VTable) -> Self {
        ForwardVTable(Ref::from(ptr).into_raw().cast())
    }

    pub fn from_forward(ptr: Raw<GcCell<()>>) -> Self {
        debug_assert!(ptr.addr().get() % 8 == 0, "ptr not alligned");
        unsafe { ForwardVTable(ptr.cast().map_addr_unchecked(|x| x | 1)) }
    }

    pub fn is_forwarded(&self) -> bool {
        (self.0.addr().get() & 1) == 1
    }

    pub fn get_vtable(self) -> Option<&'static VTable> {
        if !self.is_forwarded() {
            Some(unsafe { self.0.cast().into_ref::<'static>().into_ref() })
        } else {
            None
        }
    }

    pub fn get_forward(self) -> Option<Raw<GcCell<()>>> {
        if self.is_forwarded() {
            unsafe { Some(self.0.map_addr_unchecked(|x| x & !1).cast()) }
        } else {
            None
        }
    }
}

#[repr(C)]
pub struct GcCell<T> {
    forward_vtable: ForwardVTable,
    value: T,
}

#[repr(C)]
pub struct OwnedRoot<T> {
    node: ListNode,
    value: GcCell<T>,
}

pub struct Arena {
    unused: List<SegmentHeader>,
    nursery: Option<Raw<NurserySegment>>,
    nursery_finalizers: Vec<NuseryFinalizer>,
    retained: List<RetainedSegment>,
    owned_roots: List<OwnedRoot<()>>,
    stack_roots: Option<Raw<Scope>>,
}

impl Arena {
    pub fn new() -> Self {
        Arena {
            unused: List::new(),
            nursery: None,
            nursery_finalizers: Vec::new(),
            retained: List::new(),
            next_free_handle: None,
            handles: Vec::new(),
        }
    }

    unsafe fn unmap_segment(segment: Raw<SegmentHeader>) {
        mmap::munmap(segment.cast().as_ptr(), SEGMENT_SIZE).unwrap();
    }

    #[cold]
    unsafe fn allocate_new_segment(&mut self) -> Raw<SegmentHeader> {
        if let Some(x) = self.unused.pop_back() {
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
                self.unused.push_back(segment_b);
                return segment_a;
            }
        }
    }

    #[cold]
    unsafe fn allocate_nursery(&mut self) {
        let segment = self.allocate_new_segment();
        let segment = segment::init_nursery(segment);
        self.nursery = Some(segment);
    }

    unsafe fn get_nursery(&mut self) -> Raw<NurserySegment> {
        if let Some(x) = self.nursery {
            x
        } else {
            self.allocate_nursery();
            self.nursery.unwrap()
        }
    }

    fn get_finalizer<T>() -> Option<unsafe fn(Raw<GcCell<usize>>)> {
        if std::mem::needs_drop::<T>() {
            Some(|ptr| unsafe {
                if !ptr.as_ref().forward_vtable.is_forwarded() {
                    std::ptr::drop_in_place(ptr.as_ptr())
                }
            })
        } else {
            None
        }
    }

    pub unsafe fn alloc<T>(&mut self, value: T) -> Option<Raw<T>> {
        let layout = Layout::new::<GcCell<T>>();
        assert_eq!(
            layout.align(),
            8,
            "All values allocated in the gc need to be 8 bytes aligned"
        );
        assert!(
            layout.size() < segment::MAX_ALLOCATION,
            "Allocation exceeded maximum allowed value"
        );

        if let Some(ptr) = self.alloc_nursery(layout.size()) {
            let ptr = ptr.cast();
            self.write_young(ptr, value);
            return Some(ptr.map_ptr(map_ptr!(GcCell, value)));
        }

        self.collect_young();

        if let Some(ptr) = self.alloc_nursery(layout.size()) {
            let ptr = ptr.cast();
            self.write_young(ptr, value);
            return Some(ptr.map_ptr(map_ptr!(GcCell, value)));
        }

        todo!()
    }

    unsafe fn write_young<T>(&mut self, slot: Raw<GcCell<T>>, value: T) {
        slot.write(GcCell {
            forward_vtable: ForwardVTable(Raw::dangling()),
            value,
        });
        if mem::needs_drop::<T>() {
            self.nursery_finalizers.push(NuseryFinalizer {
                ptr: slot.cast(),
                callback: |slot: Raw<()>| unsafe {
                    if !slot
                        .cast::<GcCell<T>>()
                        .as_ref()
                        .forward_vtable
                        .is_forwarded()
                    {
                        ptr::drop_in_place(slot.as_ptr())
                    }
                },
            })
        }
    }

    unsafe fn collect_young(&mut self) {
        for i in self.nursery_finalizers.drain(..) {
            (i.callback)(i.ptr)
        }
    }

    unsafe fn alloc_size(&mut self, size: usize) -> Option<Raw<u8>> {
        if let Some(x) = self.alloc_nursery(size) {
            return Some(x);
        }

        todo!()
    }

    unsafe fn alloc_nursery(&mut self, size: usize) -> Option<Raw<u8>> {
        let mut nursery = unsafe { self.get_nursery() };
        let offset = nursery.as_ref().offset;
        if offset > (segment::SEGMENT_SIZE - size) as u32 {
            return None;
        }
        nursery.as_mut().offset += size as u32;
        Some(nursery.cast::<u8>().add(size))
    }

    unsafe fn clear(&mut self) {}
}

impl Drop for Arena {
    fn drop(&mut self) {
        unsafe {
            self.clear();
        }
    }
}

#[cfg(test)]
mod test {
    use crate::gc::{
        arena::Arena,
        segment::{self, SegmentFlags},
    };

    #[test]
    fn allocate_segment() {
        let mut arena = Arena::new();
        let segment_1 = unsafe { arena.allocate_new_segment() };
        let segment_2 = unsafe { arena.allocate_new_segment() };
        // atleast two segments are allocated, no more then four

        unsafe { assert_eq!(segment_1.as_ref().magic, segment::SEGMENT_MAGIC) };
        unsafe { assert_eq!(segment_1.as_ref().flags, SegmentFlags::empty()) };

        unsafe { Arena::unmap_segment(segment_1) };

        unsafe { assert_eq!(segment_2.as_ref().magic, segment::SEGMENT_MAGIC) };
        unsafe { assert_eq!(segment_2.as_ref().flags, SegmentFlags::empty()) };

        unsafe { Arena::unmap_segment(segment_2) };

        if let Some(segment) = unsafe { arena.unused.pop_back() } {
            unsafe { assert_eq!(segment.as_ref().magic, segment::SEGMENT_MAGIC) };
            unsafe { assert_eq!(segment.as_ref().flags, SegmentFlags::empty()) };

            unsafe { Arena::unmap_segment(segment) };
        }

        if let Some(segment) = unsafe { arena.unused.pop_back() } {
            unsafe { assert_eq!(segment.as_ref().magic, segment::SEGMENT_MAGIC) };
            unsafe { assert_eq!(segment.as_ref().flags, SegmentFlags::empty()) };

            unsafe { Arena::unmap_segment(segment) };
        }

        assert!(unsafe { arena.unused.pop_back().is_none() });
    }

    /*
    #[test]
    fn allocate_nursery() {
        #[derive(Eq, PartialEq, Debug)]
        struct Foo {
            a: u32,
            c: u32,
            b: u64,
        }

        let mut arena = Arena::new();

        let ptr = unsafe { arena.alloc::<Foo>() };
        assert_eq!(ptr.as_ptr().align_offset(std::mem::align_of::<Foo>()), 0);

        unsafe {
            ptr.as_ptr().write_volatile(Foo {
                a: 42,
                b: 72,
                c: 1000,
            })
        };

        unsafe {
            assert_eq!(
                ptr.as_ptr().read_volatile(),
                Foo {
                    a: 42,
                    b: 72,
                    c: 1000
                }
            )
        }

        let mut ptrs = Vec::with_capacity(segment::SEGMENT_SIZE);

        // Enough to ensure we fill multiple segments.
        for i in 0..segment::SEGMENT_SIZE {
            let ptr = unsafe { arena.alloc::<Foo>() };
            assert_eq!(ptr.as_ptr().align_offset(std::mem::align_of::<Foo>()), 0);

            unsafe {
                ptr.as_ptr().write_volatile(Foo {
                    a: i as u32 * 2,
                    c: segment::SEGMENT_SIZE as u32 - i as u32,
                    b: i as u64 * 4 + 1,
                })
            };

            ptrs.push(ptr);
        }

        for (i, p) in ptrs.into_iter().enumerate() {
            unsafe {
                assert_eq!(
                    p.as_ptr().read_volatile(),
                    Foo {
                        a: i as u32 * 2,
                        c: segment::SEGMENT_SIZE as u32 - i as u32,
                        b: i as u64 * 4 + 1,
                    }
                )
            }
        }

    }
    */
}
