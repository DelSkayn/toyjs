use std::{mem::MaybeUninit, pin::Pin};

mod arena;
mod cell;
pub mod constants;
mod mem;
mod ptr;
mod trace;
use cell::{ForwardKind, GcCell, TraceImpl};
use common::{map_ptr, ptr::Raw};
mod roots;
mod segment;

use cell::FreeCell;
pub use ptr::{Free, Gc, RootState, Rooted};
pub use roots::OwnedRoot;
use segment::{NurserySegment, SegmentHeader};
pub use trace::Trace;

use crate::{atom::Atom, util::list::List, value::Value};

#[derive(Clone, Copy)]
pub enum Error {
    OutOfMemory,
}

#[repr(transparent)]
pub struct Marker(Arena);

impl Marker {
    pub fn wrap_ref(this: &Arena) -> &Self {
        // Safety: This is safe because Marker is a transparent wrapper around arena.
        unsafe { std::mem::transmute(this) }
    }

    pub fn mark_value<R: RootState>(&self, value: &Value<R>) -> Result<(), Error> {
        todo!()
    }

    pub fn mark<R: RootState, T>(&self, value: &Gc<R, T>) -> Result<(), Error>
    where
        T: Trace,
        T::Rooted: TraceImpl,
    {
        todo!()
    }

    pub fn mark_atom(&self, atom: Atom) -> Result<(), Error> {
        todo!()
    }
}

pub struct Arena {
    owned_roots: List<OwnedRoot<()>>,

    unused_segments: List<SegmentHeader>,
    nusery_segment: Option<Raw<NurserySegment>>,

    free_bucket_list: [Option<Raw<FreeCell>>; constants::NUM_BUCKETS],
}

impl Arena {
    /// Safety:
    /// - Values produced by this arena may not be used with a value from another Arena object.
    /// - Arena may not be moved after it's first function is used, it must effectively be pinned
    /// but functions don't require pinning for convience in usage.
    pub unsafe fn new() -> Self {
        Arena {
            owned_roots: List::new(),
            unused_segments: List::new(),
            nusery_segment: None,
            free_bucket_list: [None; constants::NUM_BUCKETS],
        }
    }

    pub fn alloc<'a, T: TraceImpl>(&'a mut self, value: T) -> Result<Gc<Free<'a>, T>, Error> {
        assert!(std::mem::align_of::<T>() == constants::ALLOC_ALIGNMENT);
        assert!(std::mem::size_of::<T>() <= constants::MAX_ALLOCATION);
        assert!(std::mem::size_of::<T>() >= constants::MIN_ALLOCATION);

        let ptr = self
            .alloc_size(std::mem::size_of::<T>())?
            .cast::<GcCell<T>>();

        unsafe {
            ptr.write(GcCell {
                forward_kind: ForwardKind::from_kind::<T>(),
                value,
            });
        }

        let value_ptr = unsafe { ptr.map_ptr(map_ptr!(GcCell<T>, value)) };

        unsafe { Ok(Gc::from_raw(value_ptr)) }
    }

    pub fn unroot_gc<'a, T: Trace, R: RootState>(&'a self, ptr: Gc<R, T>) -> Gc<Free<'a>, T> {
        ptr.unroot(self)
    }

    /// Root a value into a pinned owned root, ensuring the value and it's gc pointers remain alive.
    pub fn root_owned<T: Trace>(&self, root: Pin<&OwnedRoot<T::Rooted>>, value: T) {
        assert!(
            root.get_ref().node.is_detached(),
            "Tried to use a owned root twice"
        );
        unsafe {
            root.get_ref()
                .value
                .get()
                .write(MaybeUninit::new(value.assert_rooted()));
        }

        self.owned_roots.push_front(OwnedRoot::erase_pin_type(root))
    }
}
