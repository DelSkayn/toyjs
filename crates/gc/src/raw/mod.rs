use std::{
    alloc::Layout,
    mem::{self, offset_of},
    ptr::{self, addr_of_mut, NonNull},
};

use bitflags::bitflags;
use common::{sassert, tassert};

mod list;
mod malloc;
mod native;

use self::list::{IntrusiveList, List, ListNode};
use crate::raw::native::{os::ProtFlags, AlignedAlloc};

const KB: usize = 1024;
const MB: usize = KB * 1024;

const SEGMENT_SIZE: usize = 4 * MB;
const MIN_ALLOC_ALIGN: usize = std::mem::size_of::<*mut ()>();
const MAX_ALLOC_ALIGN: usize = 256;
const MAX_ALLOC: usize = MB;
const PER_MAP_BYTES: usize = MIN_ALLOC_ALIGN * (i8::MAX as usize + 1);

const _: () = {
    assert!(SEGMENT_SIZE < u32::MAX as usize);
};

bitflags! {
    pub struct GcBoxFlags: u8{
        const MOVED = 0b0000_0001;
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct GcHeader(u64);

impl GcHeader {
    pub fn new<T>() -> Self {
        let v_table_ptr = GcVTable::get::<T>() as *const GcVTable;
        GcHeader(v_table_ptr as usize as u64)
    }
}

#[repr(C)]
pub struct GcBox<T> {
    header: GcHeader,
    v: T,
}

struct Marker;

pub struct GcVTable {
    trace: unsafe fn(NonNull<GcHeader>, marker: &mut Marker),
    drop: unsafe fn(NonNull<GcHeader>),
    layout: Layout,
    name: fn() -> &'static str,
}

impl GcVTable {
    pub fn get<T>() -> &'static Self {
        todo!()
    }
}

bitflags! {
    pub struct SegmentFlags: usize{
        /// Segment is part of the nursery for young objects.
        const NURSERY = 0b1;
    }
}

#[repr(C)]
pub struct SegmentHeader {
    flags: SegmentFlags,
    list: ListNode,
}

impl SegmentHeader {
    pub unsafe fn init_segment(ptr: NonNull<u8>) -> NonNull<SegmentHeader> {
        let ptr = ptr.cast::<Self>();
        ptr.as_ptr().write(SegmentHeader {
            flags: SegmentFlags::empty(),
            list: ListNode::new(),
        });
        ptr
    }
}

unsafe impl IntrusiveList for SegmentHeader {
    const OFFSET: usize = offset_of!(SegmentHeader, list);
}

#[repr(C)]
pub struct NurserySegment {
    header: SegmentHeader,
    /// current offset of where to allocate a value.
    offset: u32,
}

impl NurserySegment {
    pub const fn header_size() -> usize {
        Layout::new::<NurserySegment>().size()
    }

    pub const fn usable_space() -> usize {
        SEGMENT_SIZE - Self::header_size()
    }

    pub unsafe fn init_segment(ptr: NonNull<SegmentHeader>) -> NonNull<NurserySegment> {
        let mut ptr = ptr.cast::<Self>();
        addr_of_mut!((*ptr.as_ptr()).offset).write(SEGMENT_SIZE as u32);
        ptr.as_mut().header.flags.insert(SegmentFlags::NURSERY);
        ptr
    }
}

#[repr(C)]
pub struct RetainedSegment {
    header: SegmentHeader,
    /// current offset of where to allocate a value.
    offset: u32,
    dirty_map_bits: [u8; SEGMENT_SIZE / PER_MAP_BYTES / 8],
    map_offset: [i8; SEGMENT_SIZE / PER_MAP_BYTES],
}

impl RetainedSegment {
    pub const fn header_size() -> usize {
        Layout::new::<NurserySegment>().size()
    }

    pub const fn usable_space() -> usize {
        SEGMENT_SIZE - Self::header_size()
    }

    pub unsafe fn init_segment(ptr: NonNull<SegmentHeader>) -> NonNull<RetainedSegment> {
        let ptr = ptr.cast::<Self>();
        addr_of_mut!((*ptr.as_ptr()).offset).write(SEGMENT_SIZE as u32);
        let dirty_bits_ptr = addr_of_mut!((*ptr.as_ptr()).dirty_map_bits);
        ptr::write_bytes(dirty_bits_ptr, 0, SEGMENT_SIZE / PER_MAP_BYTES / 8);

        let map_offset_ptr = addr_of_mut!((*ptr.as_ptr()).map_offset);
        ptr::write_bytes(map_offset_ptr, 0, SEGMENT_SIZE / PER_MAP_BYTES);

        ptr
    }
}

unsafe impl IntrusiveList for NurserySegment {
    const OFFSET: usize = offset_of!(SegmentHeader, list);
}

#[derive(Default)]
pub struct Stats {
    alloc_count: usize,
    alloc_size: usize,
    off_heap: usize,
}

pub struct Arena {
    free: List<SegmentHeader>,
    full: List<SegmentHeader>,
    nursery_full: List<NurserySegment>,
    nursery_partial: Option<NonNull<NurserySegment>>,
    stats: Stats,
}

impl Arena {
    pub fn new() -> Self {
        Arena {
            free: List::new(),
            full: List::new(),
            nursery_full: List::new(),
            nursery_partial: None,
            stats: Stats::default(),
        }
    }

    pub fn alloc<T>(&mut self, object: T) {
        unsafe {
            let ptr = self.alloc_ptr::<T>();
            ptr.write(object)
        };
    }

    pub unsafe fn alloc_ptr<T>(&mut self) -> Option<NonNull<T>> {
        let _space = self.alloc_space(Layout::new::<GcBox<T>>())?;
        todo!()
    }

    pub unsafe fn alloc_space(&mut self, layout: Layout) -> Option<NonNull<u8>> {
        tassert!(layout.size() <= MAX_ALLOC);
        tassert!(layout.align() <= MAX_ALLOC_ALIGN);
        tassert!(layout.align() >= MIN_ALLOC_ALIGN);

        if let Some(nursery_partial) = self.nursery_partial {
            if let Some(x) = self.alloc_within_segment(nursery_partial, layout) {
                return Some(x);
            }

            // present segment is full, add it the full list.
            self.nursery_partial = None;

            self.nursery_full.push_back(nursery_partial);
        }

        // no present nursery segment, allocate a new one.
        let segment = if let Some(x) = self.free.pop_front() {
            x
        } else {
            // no free segments, allocate a new one.
            self.mmap_segments()?
        };

        let segment = NurserySegment::init_segment(segment);

        self.nursery_partial = Some(segment);

        self.alloc_within_segment(segment, layout)
    }

    unsafe fn mmap_segments(&mut self) -> Option<NonNull<SegmentHeader>> {
        match native::mmap_aligned(SEGMENT_SIZE, ProtFlags::READ | ProtFlags::WRITE).ok()? {
            AlignedAlloc::Single(x) => {
                let ptr = NonNull::new_unchecked(x);
                Some(SegmentHeader::init_segment(ptr))
            }
            AlignedAlloc::Double(x) => {
                let ptr = NonNull::new_unchecked(x);
                let seg_ptr = SegmentHeader::init_segment(ptr.add(SEGMENT_SIZE));
                self.free.push_back(seg_ptr);
                Some(SegmentHeader::init_segment(ptr))
            }
        }
    }

    unsafe fn alloc_within_segment(
        &mut self,
        mut segment: NonNull<NurserySegment>,
        layout: Layout,
    ) -> Option<NonNull<u8>> {
        let offset = segment.as_ref().offset;
        let aligned_offset = offset & !(layout.align() as u32 - 1);

        if aligned_offset as usize - NurserySegment::header_size() >= layout.size() {
            // There was space in the segment, allocate within.
            let new_offset = aligned_offset - layout.size() as u32;

            self.stats.alloc_size += (offset - new_offset) as usize;

            segment.as_mut().offset = new_offset;

            let ptr = segment.cast::<u8>().add(aligned_offset as usize);

            Some(ptr)
        } else {
            None
        }
    }

    pub fn collect(&mut self) {}
}
