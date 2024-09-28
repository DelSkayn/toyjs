use std::mem;

use super::{cell, segment::RetainedSegment};

pub const KB: usize = 1024;
pub const MB: usize = 1024 * KB;
/// The value which indicates the start of a segment.
pub const SEGMENT_MAGIC: usize = 0xBABA_E5CA_FEE5_BABA as usize;
/// Threshold at which a segment is considered partially filled.
pub const PARTIAL_THRESHOLD: usize = 512;
/// Size of a single segment
pub const SEGMENT_SIZE: usize = 4 * MB;
/// Amount of values that fit into a single map
pub const RETAINED_MAP_MAX: usize = i8::MAX as usize + 1;
/// Minimum aligment of an allocation.
pub const ALLOC_ALIGNMENT: usize = std::mem::size_of::<usize>();
/// The amount of bytes a map can conver.
pub const RETAINED_MAP_RANGE: usize = RETAINED_MAP_MAX * ALLOC_ALIGNMENT;
/// The amount to shift an address by to get index of the map.
pub const RETAINED_MAP_SHIFT: usize = RETAINED_MAP_RANGE.ilog2() as usize;
/// A mask to get the address of a segment.
pub const SEGMENT_MASK: usize = !(SEGMENT_SIZE - 1);
/// The maximum allocation that can be handled.
pub const MAX_ALLOCATION: usize = SEGMENT_SIZE - mem::size_of::<RetainedSegment>();
/// The minimum size of allocation.
pub const MIN_ALLOCATION: usize = std::mem::size_of::<cell::GcCell<()>>();
/// The number of allocation buckets
pub const NUM_BUCKETS: usize =
    (MAX_ALLOCATION.ilog2() as usize) - (MIN_ALLOCATION.ilog2() as usize);
