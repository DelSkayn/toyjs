use std::{
    mem::{self, MaybeUninit},
    pin::Pin,
};

mod arena;
mod cell;
pub mod constants;
mod memory;
mod ptr;
mod trace;
use cell::{ForwardKind, GcCell, TraceImpl};
use common::{map_ptr, ptr::Raw};
mod mac;
mod roots;
mod segment;

use cell::FreeCell;
pub(crate) use mac::root_owned;
use pin_project_lite::pin_project;
pub use ptr::{Free, Gc, OptionGc, OwnedRooted, RootState, Rooted};
pub use roots::OwnedRoot;
use segment::{NurserySegment, SegmentHeader};
pub use trace::{OwnedTrace, Trace};

use crate::{
    atom::{Atom, AtomMap},
    util::list::List,
    value::Value,
};

#[derive(Clone, Copy, Debug)]
pub enum Error {
    OutOfMemory,
}

#[repr(transparent)]
pub struct Marker(Arena);

impl Marker {
    pub fn wrap_ref(this: &Arena) -> &Self {
        // Safety: This is safe because Marker is a transparent wrapper around arena.
        unsafe { mem::transmute(this) }
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

    pub fn mark_option<R: RootState, T>(&self, value: &OptionGc<R, T>) -> Result<(), Error>
    where
        T: Trace,
        T::Rooted: TraceImpl,
    {
        if let Some(x) = value.as_some() {
            self.mark(x)?
        }
        Ok(())
    }

    pub fn mark_atom(&self, atom: Atom) -> Result<(), Error> {
        todo!()
    }
}

pin_project! {
    pub struct Arena {
        #[pin]
        owned_roots: List<OwnedRoot<()>>,

        #[pin]
        unused_segments: List<SegmentHeader>,
        nursery_segment: Option<Raw<NurserySegment>>,

        nursery_finalizers: Vec<Raw<GcCell<()>>>,

        free_bucket_list: [Option<Raw<FreeCell>>; constants::NUM_BUCKETS],

        atoms: AtomMap,
    }


    impl PinnedDrop for Arena {
        fn drop(this: Pin<&mut Self>) {
            unsafe{
                let this = this.get_unchecked_mut();
                this.free_all();
            }
        }
    }
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
            nursery_segment: None,
            free_bucket_list: [None; constants::NUM_BUCKETS],
            nursery_finalizers: Vec::new(),
            atoms: AtomMap::new(),
        }
    }

    pub unsafe fn init(this: Pin<&mut Arena>) {
        let this = this.project();
        List::initialize(this.owned_roots.as_ref());
        List::initialize(this.unused_segments.as_ref());
    }

    pub fn atoms(&self) -> &AtomMap {
        &self.atoms
    }

    pub fn atoms_mut(&mut self) -> &mut AtomMap {
        &mut self.atoms
    }

    pub fn alloc<'a, T: TraceImpl>(
        &'a mut self,
        value: T,
    ) -> Result<Gc<Free<'a>, T::Free<'a>>, Error> {
        assert!(std::mem::align_of::<T>() == constants::ALLOC_ALIGNMENT);
        assert!(std::mem::size_of::<T>() <= constants::MAX_ALLOCATION);
        assert!(std::mem::size_of::<T>() >= constants::MIN_ALLOCATION);

        let (nursery, ptr) = self.alloc_size(mem::size_of::<GcCell<T>>())?;
        let ptr = ptr.cast::<GcCell<T>>();

        unsafe {
            ptr.write(GcCell {
                forward_kind: ForwardKind::from_kind::<T>(),
                value,
            });
        }

        unsafe { ptr.as_borrow().forward_kind.to_kind() };

        if mem::needs_drop::<T>() && nursery {
            self.nursery_finalizers.push(ptr.cast());
        }

        let value_ptr = unsafe { ptr.map_ptr(map_ptr!(GcCell<T>, value)) };

        unsafe { Ok(Gc::from_raw(value_ptr.cast())) }
    }

    pub fn unroot_gc<'a, T: Trace, R: RootState>(&'a self, ptr: Gc<R, T>) -> Gc<Free<'a>, T> {
        ptr.unroot(self)
    }

    /// Root a value into a pinned owned root, ensuring the value and it's gc pointers remain alive.
    pub fn root_owned<T: OwnedTrace>(&self, root: Pin<&OwnedRoot<T::Owned>>, value: T) {
        assert!(
            root.get_ref().node.is_detached(),
            "Tried to use a owned root twice"
        );
        unsafe {
            root.get_ref()
                .value
                .get()
                .write(MaybeUninit::new(value.assert_owned()));
        }

        self.owned_roots.push_front(OwnedRoot::erase_pin_type(root))
    }

    pub unsafe fn free_all(&mut self) {
        for f in self.nursery_finalizers.drain(..) {
            let Some(x) = f.as_borrow().forward_kind.to_kind() else {
                panic!("nursery allocation with no kind value found");
            };

            if let Some(finalizer) = x.v_table().drop {
                (finalizer)(f.map_ptr(map_ptr!(GcCell, value)).into_mut())
            }
        }

        if let Some(x) = self.nursery_segment {
            memory::free_segment(x.cast(), constants::SEGMENT_SIZE);
        }
    }
}
