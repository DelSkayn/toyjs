use std::{
    cell::UnsafeCell,
    mem::{offset_of, MaybeUninit},
    ops::Deref,
    pin::Pin,
};

use common::{map_ptr, ptr::Ref};

use super::{
    arena::{Error, Marker},
    list::{IntrusiveList, ListNode},
    vtable::{Trace, VTable},
};

pub struct Handle<'a, T>(Ref<'a, T>);

#[repr(C)]
pub struct OwnedRoot<T> {
    pub v_table: &'static VTable,
    pub node: ListNode,
    pub value: UnsafeCell<MaybeUninit<T>>,
}

impl<T: Trace> OwnedRoot<T> {
    pub fn new() -> Self {
        OwnedRoot {
            v_table: T::v_table(),
            node: ListNode::new(),
            value: UnsafeCell::new(MaybeUninit::uninit()),
        }
    }

    pub fn erase_pin_type(p: Pin<&OwnedRoot<T>>) -> Pin<&OwnedRoot<()>> {
        // Safety: This is save as value is unsafe to access and v_table and node will be in the same
        // place in memory.
        unsafe { std::mem::transmute(p) }
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
}

impl<T> Deref for OwnedRoot<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        assert!(
            !self.node.is_detached(),
            "Tried to use root which is not, or no longer alive"
        );
        unsafe { (*self.value.get()).assume_init_ref() }
    }
}

unsafe impl<T> IntrusiveList for OwnedRoot<T> {
    const OFFSET: usize = offset_of!(OwnedRoot<T>, node);
}
