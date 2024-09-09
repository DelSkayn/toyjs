use std::alloc::Layout;

use common::ptr::Raw;

use super::arena::{Marker, Trace};

pub struct VTable {
    name: &'static str,
    trace: unsafe fn(Raw<()>, marker: &Marker),
    drop: Option<unsafe fn(Raw<()>)>,
    layout: Layout,
}

impl VTable {
    unsafe fn trace_impl<T: Trace>(ptr: Raw<()>, marker: &Marker) {
        ptr.cast::<T>().as_ref().trace(marker)
    }

    unsafe fn drop_impl<T: Trace>(ptr: Raw<()>) {
        std::ptr::drop_in_place(ptr.as_ptr());
    }

    pub fn get<T: Trace>() -> &'static VTable {
        trait HasVTable {
            const V_TABLE: VTable;
        }

        impl<T: Trace> HasVTable for T {
            const V_TABLE: VTable = VTable {
                name: T::NAME,
                trace: VTable::trace_impl::<T>,
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
