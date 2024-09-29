use std::mem;

use common::ptr::Raw;

use super::{
    cell::GcCell,
    constants::SEGMENT_SIZE,
    memory::SegmentAlloc,
    segment::{self, SegmentHeader},
    Arena, Error,
};
use crate::gc::segment::NurserySegment;

impl Arena {
    pub fn alloc_size(&mut self, cell_size: usize) -> Result<(bool, Raw<GcCell<()>>), Error> {
        if let Some(x) = self.alloc_nusery_size(cell_size)? {
            return Ok((true, x));
        }

        todo!()
    }

    /// Allocates a certain amount of bytes from the nursery segment.
    fn alloc_nusery_size(&mut self, cell_size: usize) -> Result<Option<Raw<GcCell<()>>>, Error> {
        let nursery_segment = if let Some(x) = self.nursery_segment {
            x
        } else {
            // allocate a segment
            let segment = self.get_unused_segment().ok_or(Error::OutOfMemory)?;
            let segment = unsafe { segment::init_nursery(segment) };
            self.nursery_segment = Some(segment);
            segment
        };

        unsafe {
            let mut nursery_segment = nursery_segment.into_mut();
            let offset = nursery_segment.as_borrow().offset;
            let size = cell_size as u32;
            if offset as usize - mem::size_of::<NurserySegment>() < size as usize {
                return Ok(None);
            }

            let new_offset = offset - size;
            nursery_segment.as_borrow_mut().offset = new_offset;
            let ptr = nursery_segment
                .into_raw()
                .cast::<u8>()
                .add_bytes(new_offset as usize);
            Ok(Some(ptr.cast()))
        }
    }

    #[cold]
    fn get_unused_segment(&self) -> Option<Raw<SegmentHeader>> {
        if let Some(x) = self.unused_segments.pop_front() {
            return Some(x);
        }

        unsafe {
            match super::memory::alloc_segment(SEGMENT_SIZE)? {
                SegmentAlloc::Single(x) => Some(segment::init_segment(x)),
                SegmentAlloc::Double(x) => {
                    let a = segment::init_segment(x);
                    let b = segment::init_segment(x.add_bytes(SEGMENT_SIZE));
                    self.unused_segments.push_back_raw(b);
                    Some(a)
                }
            }
        }
    }
}
