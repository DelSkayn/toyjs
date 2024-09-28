use std::{alloc::Layout, ptr};

use common::ptr::{Mut, Raw, Ref};

use super::arena::{Error, Marker};

pub unsafe trait Trace {
    const NAME: &str;
    const NEEDS_TRACE: bool;

    type Rooted: Trace + 'static;
    type Free<'a>: Trace;

    fn v_table() -> &'static VTable
    where
        Self: Sized,
    {
        VTable::get::<Self>()
    }

    fn trace(&self, trace: &Marker) -> Result<(), Error>;

    unsafe fn assert_rooted(self) -> Self::Rooted
    where
        Self: Sized,
    {
        assert!(
            const { std::mem::size_of::<Self>() == std::mem::size_of::<Self::Rooted>() },
            "Invalid rooted type definition, size_of<T>() != size_of::<T::Rooted>()"
        );
        assert!(
            const { std::mem::align_of::<Self>() == std::mem::align_of::<Self::Rooted>() },
            "Invalid rooted type definition, align_of<T>() != align_of::<T::Rooted>()"
        );
        let r = unsafe { Ref::from(&self).cast::<Self::Rooted>().read() };
        std::mem::forget(self);
        r
    }

    unsafe fn assert_rooted_ref(&self) -> &Self::Rooted
    where
        Self: Sized,
    {
        assert!(
            const { std::mem::size_of::<Self>() == std::mem::size_of::<Self::Rooted>() },
            "Invalid rooted type definition, size_of<T>() != size_of::<T::Rooted>()"
        );
        assert!(
            const { std::mem::align_of::<Self>() == std::mem::align_of::<Self::Rooted>() },
            "Invalid rooted type definition, align_of<T>() != align_of::<T::Rooted>()"
        );
        Ref::from(self).cast::<Self::Rooted>().into_borrow()
    }
}

pub struct VTable {
    pub name: &'static str,
    // Point to the value itself so that we can reuse this v_table for non-gc-allocd values.
    pub trace: Option<unsafe fn(Ref<u8>, marker: &Marker) -> Result<(), Error>>,
    pub drop: Option<unsafe fn(Mut<u8>)>,
    pub layout: Layout,
}

impl VTable {
    unsafe fn trace_impl<T: Trace>(ptr: Ref<u8>, marker: &Marker) -> Result<(), Error> {
        let ptr = ptr.cast::<T>();
        ptr.as_borrow().trace(marker)
    }

    unsafe fn drop_impl<T: Trace>(ptr: Mut<u8>) {
        let ptr = ptr.cast::<T>();
        ptr::drop_in_place(ptr.as_ptr())
    }

    pub fn get<T: Trace>() -> &'static VTable {
        trait HasVTable {
            const V_TABLE: VTable;
        }

        impl<T: Trace> HasVTable for T {
            const V_TABLE: VTable = VTable {
                name: T::NAME,
                trace: if T::NEEDS_TRACE {
                    Some(VTable::trace_impl::<T>)
                } else {
                    None
                },
                drop: if std::mem::needs_drop::<T>() {
                    Some(VTable::drop_impl::<T>)
                } else {
                    None
                },
                layout: std::alloc::Layout::new::<T>(),
            };
        }

        &T::V_TABLE
    }
}

#[derive(Clone, Copy)]
pub struct ForwardVTable(Raw<()>);

impl ForwardVTable {
    pub fn from_vtable(ptr: &'static VTable) -> Self {
        ForwardVTable(Ref::from(ptr).into_raw().cast())
    }

    pub fn from_forward(ptr: Raw<GcCell<()>>) -> Self {
        debug_assert!(ptr.addr().get() % 8 == 0, "ptr not alligned");
        unsafe { ForwardVTable(ptr.cast().map_addr_unchecked(|x| x | 1)) }
    }

    pub fn is_forwarded(&self) -> bool {
        (self.0.addr().get() & 1) == 1
    }

    pub fn get_vtable(self) -> Option<&'static VTable> {
        if !self.is_forwarded() {
            Some(unsafe { self.0.cast().into_ref::<'static>().into_borrow() })
        } else {
            None
        }
    }

    pub fn get_forward(self) -> Option<Raw<GcCell<()>>> {
        if self.is_forwarded() {
            unsafe { Some(self.0.map_addr_unchecked(|x| x & !1).cast()) }
        } else {
            None
        }
    }
}

#[repr(C)]
pub struct GcCell<T> {
    pub forward_vtable: ForwardVTable,
    #[cfg(debug_assertions)]
    pub magic: u64,
    pub value: GcCellValue<T>,
}

impl<T: Trace> GcCell<T> {
    #[cfg(debug_assertions)]
    pub const MAGIC: u64 = 0x5C5C_C377_5C5C_C377 as u64;

    pub fn new(value: T) -> Self {
        GcCell {
            forward_vtable: ForwardVTable::from_vtable(T::v_table()),
            #[cfg(debug_assertions)]
            magic: Self::MAGIC,
            value: GcCellValue {
                value: ManuallyDrop::new(value),
            },
        }
    }
}

impl<T> GcCell<T> {
    pub unsafe fn trace(this: Ref<Self>, marker: &Marker) -> Result<(), Error> {
        let Some(x) = this.as_borrow().forward_vtable.get_vtable() else {
            // already forwarded, nothing to do.
            return Ok(());
        };
        if let Some(x) = x.trace {
            return unsafe { (x)(this.cast(), marker) };
        }
        Ok(())
    }

    pub unsafe fn drop_in_place(this: Mut<Self>) {
        let vtable = this.as_borrow().forward_vtable.get_vtable().unwrap();
        if let Some(drop) = vtable.drop {
            unsafe { (drop)(this.cast()) };
        }
    }

    pub unsafe fn forward(src: Raw<Self>, dst: Raw<Self>, next: Option<Raw<GcCell<()>>>) {
        std::ptr::copy_nonoverlapping(src.as_ptr(), dst.as_ptr(), std::mem::size_of::<Self>());
        src.map_ptr(map_ptr!(self, forward_vtable))
            .write(ForwardVTable::from_forward(dst.cast()));
        src.map_ptr(map_ptr!(self, value))
            .write(GcCellValue { next });
    }
}

pub union GcCellValue<T> {
    pub next: Option<Raw<GcCell<()>>>,
    pub value: ManuallyDrop<T>,
}
