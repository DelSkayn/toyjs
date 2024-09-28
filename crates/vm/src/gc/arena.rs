use common::ptr::Raw;

use super::{
    cell::GcCell,
    constants::{self, SEGMENT_SIZE},
    mem::SegmentAlloc,
    segment::{self, SegmentHeader},
    Arena, Error,
};

impl Arena {
    pub fn alloc_size(&mut self, cell_size: usize) -> Result<Raw<GcCell<()>>, Error> {
        if let Some(x) = self.alloc_nusery_size(cell_size)? {
            return Ok(x);
        }

        todo!()
    }

    /// Allocates a certain amount of bytes from the nursery segment.
    fn alloc_nusery_size(&mut self, cell_size: usize) -> Result<Option<Raw<GcCell<()>>>, Error> {
        let nursery_segment = if let Some(x) = self.nusery_segment {
            x
        } else {
            // allocate a segment
            let segment = self.get_unused_segment().ok_or(Error::OutOfMemory)?;
            let segment = unsafe { segment::init_nursery(segment) };
            self.nusery_segment = Some(segment);
            segment
        };

        unsafe {
            let mut nursery_segment = nursery_segment.into_mut();
            let offset = nursery_segment.as_borrow().offset;
            let size = cell_size as u32;
            // shouldn't overflow since MAX_ALLOCATION < (u32::MAX / 2)
            let new_offset = offset + size;
            if new_offset > constants::SEGMENT_SIZE as u32 {
                return Ok(None);
            }
            nursery_segment.as_borrow_mut().offset = new_offset;
            let ptr = nursery_segment.cast::<u8>().add(new_offset as usize);
            Ok(Some(ptr.into_raw().cast()))
        }
    }

    #[cold]
    fn get_unused_segment(&self) -> Option<Raw<SegmentHeader>> {
        if let Some(x) = self.unused_segments.pop_front() {
            return Some(x);
        }

        unsafe {
            match super::mem::alloc_segment(SEGMENT_SIZE)? {
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
