use std::{mem, pin::Pin};

use super::{Arena, Error, Marker, SomeGcPtr};
use crate::gc::{arena::TracingState, cell::GcCell, segment, vtable::VTable};

impl Arena {
    pub(super) unsafe fn collect_young(&mut self) -> Result<(), Error> {
        self.state = TracingState::TracingYoung;

        // Trace all owned roots,
        let mut cursor = Pin::new_unchecked(&self.owned_roots).cursor_next();
        while let Some(x) = cursor.get() {
            x.trace(Marker::wrap_arena(self))?;
            self.mark_pending()?;
            cursor = cursor.next();
        }

        // Trace all stack roots,
        // TODO

        // Trace retained values.
        let mut cursor = Pin::new_unchecked(&self.old_segments).cursor_next();
        while let Some(x) = cursor.get_raw() {
            cursor = cursor.next();
            segment::for_each_dirty_map_clear(x.into_mut(), |ptr| {
                let ptr = ptr.cast::<GcCell<()>>();
                let v_table = ptr.as_borrow().forward_vtable.get_vtable().unwrap();
                GcCell::<()>::trace(ptr.into_ref(), Marker::wrap_arena(self));
                return v_table.layout.size();
            })
        }

        for x in self.young_finalizers.drain(..) {
            GcCell::<()>::drop_in_place(x.into_mut());
        }

        self.young_segment.unwrap().as_borrow_mut().offset = segment::SEGMENT_SIZE as u32;

        self.state = TracingState::None;
        Ok(())
    }

    pub unsafe fn mark_pending(&self) -> Result<(), Error> {
        while let Some(x) = self.pending.take() {
            let ptr = x.as_borrow().forward_vtable.get_forward().unwrap();
            self.pending.set(x.as_borrow().value.next);

            GcCell::trace(ptr.into_ref(), Marker::wrap_arena(self))?;
        }
        Ok(())
    }

    pub(super) unsafe fn mark_young(
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
