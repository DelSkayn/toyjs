use std::{
    alloc,
    cell::Cell,
    mem,
    ptr::{self, NonNull},
};

/// An object which can be used to increment the reference count of
/// any Rc object regardless of the contained value.
#[repr(C)]
pub struct RcCount {
    count: u64,
}

impl RcCount {
    /// Increment the reference count of pointer representing a Rc<T>,
    /// # Safety
    /// Pointer must be a valid pointer as obtained from `Rc::to_raw`.
    pub unsafe fn incr_raw(ptr: *mut ()) {
        let ptr = ptr as *mut RcCount;
        (*ptr).count += 1;
    }
}

#[repr(C)]
pub(crate) struct RcVal<T> {
    count: Cell<usize>,
    value: T,
}

/// A struct for manual reference counting, it is the users responsibility to
/// make sure the reference count remains correct.
///
/// Unlike the Rc from the standart library this pointer will not increment or decrement
/// reference count on its own.
///
/// As such constructing `Rc` from raw pointers is save as using the pointer in any
/// other way the constructing and deconstructing is unsafe.
pub struct ManualRc<T> {
    ptr: NonNull<RcVal<T>>,
}

impl<T> Clone for ManualRc<T> {
    fn clone(&self) -> Self {
        ManualRc { ptr: self.ptr }
    }
}

impl<T> Copy for ManualRc<T> {}

impl<T> ManualRc<T> {
    pub fn new(value: T) -> Self {
        unsafe {
            let layout = alloc::Layout::new::<RcVal<T>>();
            let alloc = alloc::alloc(layout) as *mut RcVal<T>;
            alloc.write(RcVal {
                count: Cell::new(1),
                value,
            });
            ManualRc {
                ptr: NonNull::new_unchecked(alloc),
            }
        }
    }

    #[inline(always)]
    pub fn from_raw(ptr: *mut ()) -> Self {
        ManualRc {
            ptr: NonNull::new(ptr.cast()).unwrap(),
        }
    }

    #[inline(always)]
    pub fn to_raw(self) -> *mut () {
        self.ptr.as_ptr().cast()
    }

    #[inline(always)]
    pub unsafe fn incr(&self) {
        self.ptr
            .as_ref()
            .count
            .set(self.ptr.as_ref().count.get() + 1);
    }

    #[inline(always)]
    pub unsafe fn value(&self) -> &T {
        &(self.ptr.as_ref().value)
    }

    #[inline]
    pub unsafe fn drop(self) {
        let val = self.ptr.as_ref();
        debug_assert_ne!(val.count.get(), 0);
        val.count.set(val.count.get() - 1);
        if val.count.get() == 0 {
            ptr::drop_in_place(&mut (*self.ptr.as_ptr()).value);
            let layout = alloc::Layout::new::<ManualRc<T>>();
            std::alloc::dealloc(self.ptr.as_ptr().cast(), layout)
        }
    }
}

impl<T: Clone + 'static> ManualRc<T> {
    pub unsafe fn deep_clone(self) -> Self {
        ManualRc::new(self.value().clone())
    }
}
