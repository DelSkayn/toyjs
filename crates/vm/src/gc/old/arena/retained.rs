use std::{mem, pin::Pin};

use common::ptr::Raw;

use super::{Arena, Error, Marker, SomeGcPtr};
use crate::gc::{arena::TracingState, cell::GcCell, segment, vtable::VTable};

impl Arena {
    pub unsafe fn search(size: usize) -> Raw<u8> {
        let bucket = size.ilog2();
    }

    pub(super) unsafe fn collect_full(&mut self) -> Result<(), Error> {
        std::mem::swap(&mut self.old_segments, &mut self.pending_retained);

        // Trace all owned roots,
        let mut cursor = Pin::new_unchecked(&self.owned_roots).cursor_next();
        while let Some(x) = cursor.get() {
            x.trace(Marker::wrap_arena(self))?;
            self.mark_pending()?;
            cursor = cursor.next();
        }

        todo!()
    }

    pub(super) unsafe fn mark_full(
        &self,
        v_table: &VTable,
        ptr: &SomeGcPtr<()>,
    ) -> Result<(), Error> {
        let size = v_table.layout.size() + mem::size_of::<GcCell<()>>();
        let segment = self.old_segments.front().and_then(|x| {
            segment::alloc_nursery(x.cast().into_mut(), size).or_else(|| {
                segment::alloc_nursery(self.old_segments.back().unwrap().cast().into_mut(), size)
            })
        });

        let alloc = segment.unwrap_or_else(|| {
            let retained = self.segment_retained_allocate();
            self.old_segments.push_front_raw(retained);
            segment::alloc_nursery(retained.cast().into_mut(), size).unwrap()
        });

        GcCell::<()>::forward(ptr.0.get().cast(), alloc.cast(), self.pending.get());

        segment::insert_retained_alloc(alloc, size);

        self.pending.set(Some(alloc.cast()));

        Ok(())
    }
}
