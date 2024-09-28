use std::mem::ManuallyDrop;

use common::{
    map_ptr,
    ptr::{Mut, Raw, Ref},
};

use super::{
    arena::{Error, Marker},
    vtable::{Trace, VTable},
};

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
