use std::mem::{self, offset_of};

use bitflags::bitflags;
use common::{bitmap::BitMap, map_ptr, ptr::Raw};

use super::{
    list::{IntrusiveList, ListNode},
    MB,
};

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct MapIdx(pub u32);

/// The value which indicates the start of a segment.
pub const SEGMENT_MAGIC: usize = 0xBABA_E5CA_FEE5_BABA as usize;
/// Threshold at which a segment is considered partially filled.
pub const PARTIAL_THRESHOLD: usize = 512;
/// Size of a single segment
pub const SEGMENT_SIZE: usize = 4 * MB;
/// Amount of values that fit into a single map
pub const RETAINED_MAP_MAX: usize = i8::MAX as usize + 1;
/// Minimum aligment of an allocation.
pub const MIN_ALIGNMENT: usize = std::mem::size_of::<usize>();
/// The amount of bytes a map can conver.
pub const RETAINED_MAP_RANGE: usize = RETAINED_MAP_MAX * MIN_ALIGNMENT;
/// The amount to shift an address by to get index of the map.
pub const RETAINED_MAP_SHIFT: usize = RETAINED_MAP_RANGE.ilog2() as usize;
/// A mask to get the address of a segment.
pub const SEGMENT_MASK: usize = !(SEGMENT_SIZE - 1);
/// The maximum allocation that can be handled.
pub const MAX_ALLOCATION: usize = SEGMENT_SIZE - mem::size_of::<RetainedSegment>();

bitflags! {
    #[derive(Clone,Copy,Debug,Eq, PartialEq)]
    pub struct SegmentFlags: usize{
        /// Segment is actively used.
        const USED = 0b01;
        /// Segment is part of the nursery for young objects.
        const NURSERY = 0b10;
    }
}

/// The header for all segments.
#[repr(C)]
pub struct SegmentHeader {
    pub magic: usize,
    pub flags: SegmentFlags,
    pub list: ListNode,
}

/// A segment for young objects.
#[repr(C)]
pub struct NurserySegment {
    pub header: SegmentHeader,
    /// Offset into the segment from where to allocate.
    pub offset: u32,
}

/// A segment for retained objects.
#[repr(C)]
pub struct RetainedSegment {
    header: SegmentHeader,
    /// Bitmap marking a map as having a dirty value.
    dirty: BitMap<usize, { SEGMENT_SIZE / std::mem::size_of::<usize>() / 8 }>,
    /// Map for finding start of the start of allocations.
    maps: [i8; SEGMENT_SIZE / RETAINED_MAP_RANGE],
}

/// Returns the segment header of any gc allocated pointer.
pub unsafe fn ptr_to_segment_header(ptr: Raw<u8>) -> Raw<SegmentHeader> {
    let ptr = ptr.map_addr_unchecked(|x| x & SEGMENT_MASK);
    debug_assert_eq!(
        ptr.cast::<usize>().read(),
        SEGMENT_MAGIC,
        "Tried to get the segment header of a non-gc pointer"
    );
    ptr.cast()
}

/// Initialize a pointer to a
pub unsafe fn init_segment(ptr: Raw<u8>) -> Raw<SegmentHeader> {
    let res = ptr.cast::<SegmentHeader>();
    debug_assert_eq!(
        ptr.addr().get() % SEGMENT_SIZE,
        0,
        "Tried to initialize invalidly aligned segment pointer"
    );
    res.write(SegmentHeader {
        magic: SEGMENT_MAGIC,
        flags: SegmentFlags::empty(),
        list: ListNode::new(),
    });
    res
}

pub unsafe fn init_nursery(ptr: Raw<SegmentHeader>) -> Raw<NurserySegment> {
    let mut ptr = ptr.cast::<NurserySegment>();
    ptr.map_ptr(map_ptr!(NurserySegment, offset))
        .write(SEGMENT_SIZE as u32);
    ptr.as_mut()
        .header
        .flags
        .insert(SegmentFlags::NURSERY | SegmentFlags::USED);
    ptr
}

pub unsafe fn init_retained(ptr: Raw<SegmentHeader>) -> Raw<RetainedSegment> {
    let mut ptr = ptr.cast::<RetainedSegment>();
    ptr.map_ptr(map_ptr!(RetainedSegment, dirty)).fill(0);
    ptr.map_ptr(map_ptr!(RetainedSegment, maps))
        .fill(i8::MIN as u8);
    ptr.as_mut().header.flags.insert(SegmentFlags::USED);
    ptr
}

pub unsafe fn ptr_to_map_index(ptr: Raw<u8>) -> MapIdx {
    MapIdx(((ptr.addr().get() & !SEGMENT_MASK) >> RETAINED_MAP_SHIFT) as u32)
}

pub unsafe fn ptr_to_map_offset(ptr: Raw<u8>) -> u8 {
    ((ptr.addr().get() & (1 << RETAINED_MAP_SHIFT) - 1) >> 3) as u8
}

pub unsafe fn map_index_to_pointer(segment: Raw<RetainedSegment>, idx: MapIdx) -> Raw<u8> {
    segment
        .cast::<u8>()
        .add((idx.0 as usize) * RETAINED_MAP_RANGE)
}

pub unsafe fn find_first_allocation_in_map(
    segment: Raw<RetainedSegment>,
    mut map: MapIdx,
) -> Option<Raw<u8>> {
    let maps = segment.map_ptr(map_ptr!(NurserySegment, maps));
    loop {
        let offset = maps.cast::<i8>().add(map.0 as usize).read();
        if offset == i8::MIN {
            return None;
        }
        if offset < 0 {
            map.0 -= (-offset) as u32;
            continue;
        }
        return Some(map_index_to_pointer(segment, map).add((offset as usize) * MIN_ALIGNMENT));
    }
}

pub unsafe fn insert_retained_alloc(ptr: Raw<u8>, size: usize) {
    let segment_ptr = ptr_to_segment_header(ptr);
    debug_assert!(
        segment_ptr.as_ref().flags.contains(SegmentFlags::USED),
        "Tried to allocated in a segment not in use"
    );
    debug_assert!(
        !segment_ptr.as_ref().flags.contains(SegmentFlags::NURSERY),
        "Tried to alloc a retained value in a nursery segment"
    );
    debug_assert_eq!(ptr.addr().get() % 8, 0, "Pointer not properly aligned");
    let segment_ptr = segment_ptr.cast::<RetainedSegment>();

    let idx = ptr_to_map_index(ptr);
    let maps = segment_ptr.map_ptr(map_ptr!(RetainedSegment, maps));
    let map_ptr = maps.cast::<i8>().add(idx.0 as usize);
    let map_value = map_ptr.read();
    let map_offset = ptr_to_map_offset(ptr);

    if map_value == i8::MIN {
        map_ptr.write(map_offset as i8);
    }

    // update maps if this value moves past a border.
    let mut map_offset = 1;
    let mut last_offset = map_offset + size;
    while last_offset > RETAINED_MAP_RANGE {
        last_offset -= RETAINED_MAP_RANGE;
        map_ptr.add(map_offset).write(-(map_offset.min(127) as i8));
        map_offset += 1;
    }
}

unsafe impl IntrusiveList for SegmentHeader {
    const OFFSET: usize = offset_of!(SegmentHeader, list);
}

unsafe impl IntrusiveList for NurserySegment {
    const OFFSET: usize = offset_of!(SegmentHeader, list);
}

unsafe impl IntrusiveList for RetainedSegment {
    const OFFSET: usize = offset_of!(SegmentHeader, list);
}

#[cfg(test)]
mod test {
    use std::alloc::Layout;

    use common::ptr::Raw;

    use super::{init_retained, insert_retained_alloc};
    use crate::gc::segment::{
        find_first_allocation_in_map, init_segment, ptr_to_map_index, ptr_to_map_offset,
        ptr_to_segment_header, MapIdx, MIN_ALIGNMENT, RETAINED_MAP_RANGE, SEGMENT_SIZE,
    };

    #[test]
    fn sanity_segment_map() {
        unsafe {
            // Allocate a segment so that our test pointers have a provenance.
            let layout = Layout::from_size_align(SEGMENT_SIZE, SEGMENT_SIZE).unwrap();
            let segment = Raw::from_ptr(std::alloc::alloc(layout)).unwrap();

            assert_eq!(ptr_to_map_index(segment.cast()), MapIdx(0));
            assert_eq!(
                ptr_to_map_index(segment.add(SEGMENT_SIZE - 1).cast()),
                MapIdx(((SEGMENT_SIZE / RETAINED_MAP_RANGE) - 1) as u32)
            );
            std::alloc::dealloc(segment.as_ptr(), layout);
        }
    }

    #[test]
    fn sanity_ptr_to_segment() {
        unsafe {
            let layout = Layout::from_size_align(SEGMENT_SIZE, SEGMENT_SIZE).unwrap();
            let segment = Raw::from_ptr(std::alloc::alloc(layout)).unwrap();
            let segment = init_segment(segment);

            let ptr = segment.cast::<u8>();
            for i in 0..SEGMENT_SIZE {
                assert_eq!(ptr_to_segment_header(ptr.add(i)).addr(), segment.addr());
            }

            std::alloc::dealloc(segment.cast().as_ptr(), layout);
        }
    }

    #[test]
    fn sanity_map_offset() {
        unsafe {
            // Allocate a segment so that our test pointers have a provenance.
            let layout = Layout::from_size_align(SEGMENT_SIZE, SEGMENT_SIZE).unwrap();
            let segment = Raw::from_ptr(std::alloc::alloc(layout)).unwrap();

            assert_eq!(ptr_to_map_index(segment.cast()), MapIdx(0));
            let ptr = segment.cast::<u8>();
            for i in 0..RETAINED_MAP_RANGE {
                assert_eq!(ptr_to_map_offset(ptr.add(i)), (i / MIN_ALIGNMENT) as u8);
            }
        }
    }

    #[test]
    fn sanity_retained_alloc() {
        unsafe {
            let layout = Layout::from_size_align(SEGMENT_SIZE, SEGMENT_SIZE).unwrap();
            let segment = Raw::from_ptr(std::alloc::alloc(layout)).unwrap();
            let segment = init_segment(segment);
            let segment = init_retained(segment);

            let test_alloc_ptr = segment
                .cast::<u8>()
                .add(RETAINED_MAP_RANGE * 2)
                .add(RETAINED_MAP_RANGE / 2);

            insert_retained_alloc(test_alloc_ptr, RETAINED_MAP_RANGE);

            assert_eq!(
                segment.as_ref().maps[2] as usize * MIN_ALIGNMENT,
                RETAINED_MAP_RANGE / 2
            );
            assert_eq!(segment.as_ref().maps[3], -1);

            assert_eq!(
                Some(test_alloc_ptr),
                find_first_allocation_in_map(segment, MapIdx(3))
            );

            let test_alloc_ptr = segment
                .cast::<u8>()
                .add(RETAINED_MAP_RANGE * 4)
                .add(RETAINED_MAP_RANGE - 8);

            insert_retained_alloc(test_alloc_ptr, RETAINED_MAP_RANGE * 4);
            assert_eq!(
                segment.as_ref().maps[4] as usize * MIN_ALIGNMENT,
                RETAINED_MAP_RANGE - 8
            );
            assert_eq!(segment.as_ref().maps[5], -1);
            assert_eq!(segment.as_ref().maps[6], -2);
            assert_eq!(segment.as_ref().maps[7], -3);
            assert_eq!(segment.as_ref().maps[8], -4);

            assert_eq!(
                Some(test_alloc_ptr),
                find_first_allocation_in_map(segment, MapIdx(4))
            );

            assert_eq!(
                Some(test_alloc_ptr),
                find_first_allocation_in_map(segment, MapIdx(6))
            );

            assert_eq!(
                Some(test_alloc_ptr),
                find_first_allocation_in_map(segment, MapIdx(8))
            );
        }
    }
}
