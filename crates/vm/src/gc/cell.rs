use common::ptr::{Raw, Ref};

use super::{Error, Marker, RootState, Rooted, Trace};
use crate::{
    gc::constants::ALLOC_ALIGNMENT,
    object::{Object, PropertyMap, Shape},
};

const _: () = const {
    assert!(
        ALLOC_ALIGNMENT > 2,
        "needs to be valid to ensure thet the lower bit of size is always 0 for ForwardKind impl"
    );
};

pub trait TraceImpl: Trace {
    const KIND: TraceKind;
}

type ForwardingPtr = Raw<GcCell<()>>;

/// A pointer indicating the forwarding of a value.
#[derive(Clone, Copy)]
pub struct ForwardPtr(ForwardingPtr);

impl From<Raw<GcCell<()>>> for ForwardPtr {
    fn from(value: Raw<GcCell<()>>) -> Self {
        debug_assert!(value.addr().get() & 1 == 0);
        let addr = unsafe {
            value.map_addr_unchecked(|x| {
                debug_assert!(x & 1 == 0);
                x ^ 1
            })
        };
        ForwardPtr(addr)
    }
}
impl From<ForwardPtr> for Raw<GcCell<()>> {
    fn from(value: ForwardPtr) -> Self {
        let addr = unsafe {
            value.0.map_addr_unchecked(|x| {
                debug_assert!(x & 1 == 1);
                x ^ 1
            })
        };
        addr
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct KindSize {
    kind: TraceKind,
    size: u32,
}

impl KindSize {
    /// The maximum size that can be stored within this kindsize.
    pub const MAX_SIZE: usize = u32::MAX as usize;

    pub fn from_kind<T: TraceImpl>() -> Self {
        assert!(const { std::mem::size_of::<T>() <= KindSize::MAX_SIZE });
        assert!(const { std::mem::align_of::<T>() >= ALLOC_ALIGNMENT });

        let size = std::mem::size_of::<T>() as u32;
        KindSize {
            kind: T::KIND,
            size,
        }
    }

    /// Returns the v_table for the type associated with struct.
    pub fn v_table(&self) -> &'static VTable {
        // Safety: impl_trace macro ensures that all TraceKind's map to valid V_TABLES>
        unsafe { VTABLE_TABLE.get_unchecked(self.kind as u8 as usize) }
    }

    pub fn size(&self) -> usize {
        self.size as usize
    }
}

#[derive(Clone, Copy)]
pub union ForwardKind {
    forward: ForwardPtr,
    kind: KindSize,
}

impl ForwardKind {
    pub fn from_kind<T: TraceImpl>() -> Self {
        ForwardKind {
            kind: KindSize::from_kind::<T>(),
        }
    }

    pub fn from_forward<T>(raw: ForwardingPtr) -> Self {
        ForwardKind {
            forward: ForwardPtr::from(raw),
        }
    }

    pub fn is_forward(&self) -> bool {
        unsafe { (self.forward.0.addr().get() & 1) == 1 }
    }

    pub fn is_kind(&self) -> bool {
        unsafe { (self.forward.0.addr().get() & 1) == 0 }
    }

    pub fn to_forward(self) -> Option<ForwardPtr> {
        unsafe { self.is_forward().then(|| self.forward) }
    }

    pub fn to_kind(self) -> Option<KindSize> {
        unsafe { self.is_kind().then(|| self.kind) }
    }
}

#[repr(C)]
/// A wrapper around a value allocated in the GC storing a v-table as well as the size of the
/// value.
pub struct GcCell<T> {
    pub(super) forward_kind: ForwardKind,
    pub(super) value: T,
}

/// Cell used to make the heap parseable. Placed in empty spots.
pub(super) struct FreeCell {
    next: Option<Raw<GcCell<FreeCell>>>,
}

unsafe impl Trace for FreeCell {
    type Free<'a> = FreeCell;
    type Rooted = FreeCell;

    const NEEDS_TRACE: bool = false;

    fn trace(&self, _marker: &Marker) -> Result<(), Error> {
        panic!("SizeCell::trace should never be called")
    }
}

pub struct VTable {
    pub(super) needs_trace: bool,
    pub(super) trace: Option<unsafe fn(this: Ref<()>, marker: &Marker) -> Result<(), Error>>,
}

impl VTable {
    unsafe fn trace_impl<T: Trace>(this: Ref<()>, marker: &Marker) -> Result<(), Error> {
        this.cast::<T>().as_borrow().trace(marker)
    }

    pub const fn get<T: Trace>() -> Self {
        VTable {
            needs_trace: T::NEEDS_TRACE,
            trace: if T::NEEDS_TRACE {
                Some(Self::trace_impl::<T>)
            } else {
                None
            },
        }
    }

    pub const fn get_ref<T: Trace>() -> &'static Self {
        trait HasVTable {
            const V_TABLE: VTable;
        }

        impl<T: Trace> HasVTable for T {
            const V_TABLE: VTable = VTable::get::<T>();
        }

        &T::V_TABLE
    }
}

macro_rules! impl_trace {
    ($($name:ident$(<$gen:ident>)?),*) => {
        #[repr(u8)]
        #[derive(Clone,Copy)]
        pub enum TraceKind{
            $($name),*
        }

        static VTABLE_TABLE: &[VTable] = &[
            $(
                VTable::get::<$name$(<$gen>)?>()
            ),*
        ];

        $(
            impl TraceImpl for $name$(<$gen>)? {
                const KIND: TraceKind = TraceKind::$name;
            }
        )*
    };
}

impl_trace! {
    FreeCell,
    Shape<Rooted>,
    Object<Rooted>,
    PropertyMap
}
