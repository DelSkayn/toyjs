use std::{
    mem::{offset_of, MaybeUninit},
    ops::Deref,
    pin::Pin,
};

use common::{
    map_ptr,
    ptr::{RawCell, Ref},
};

use super::Arena;
use crate::{
    gc::{cell::VTable, Error, Marker, Trace},
    util::list::{IntrusiveList, ListNode},
};

pub struct Handle<'a, T>(Ref<'a, T>);

#[repr(C)]
pub struct OwnedRoot<T> {
    pub v_table: &'static VTable,
    pub node: ListNode,
    pub value: RawCell<MaybeUninit<T>>,
}

impl<T: Trace> OwnedRoot<T> {
    pub fn new() -> Self {
        OwnedRoot {
            v_table: VTable::get_ref::<T>(),
            node: ListNode::new(),
            value: RawCell::new(MaybeUninit::uninit()),
        }
    }
}

impl<T> OwnedRoot<T> {
    pub fn trace(&self, marker: &Marker) -> Result<(), Error> {
        if let Some(x) = self.v_table.trace {
            unsafe {
                let ptr = Ref::from(self).map_ptr(map_ptr!(Self, value)).cast();
                return x(ptr, marker);
            }
        }
        Ok(())
    }

    pub fn erase_pin_type(p: Pin<&OwnedRoot<T>>) -> Pin<&OwnedRoot<()>> {
        // Safety: This is save as value is unsafe to access and v_table and node will be in the same
        // place in memory.
        unsafe { std::mem::transmute(p) }
    }

    /// Borrow the rooted value
    ///
    /// You need to provide a borrow to the arena to proof your not running a collection while the
    /// value is borrowed.
    pub fn borrow_mut<'a>(p: Pin<&'a mut Self>, arena: &'a Arena) -> &'a mut T {
        unsafe {
            let this = p.get_unchecked_mut();
            assert!(
                !this.node.is_detached(),
                "Tried to use root which is not, or no longer alive"
            );
            let _ = arena;
            this.value
                .get()
                .into_mut()
                .into_borrow_mut()
                .assume_init_mut()
        }
    }
}

impl<T> Deref for OwnedRoot<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        assert!(
            !self.node.is_detached(),
            "Tried to use root which is not, or no longer alive"
        );
        unsafe { self.value.get().into_ref().into_borrow().assume_init_ref() }
    }
}

unsafe impl<T> IntrusiveList for OwnedRoot<T> {
    const OFFSET: usize = offset_of!(OwnedRoot<T>, node);
}
