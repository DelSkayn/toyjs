use std::{alloc, cell, mem};

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
struct RcVal<T> {
    count: u64,
    value: mem::ManuallyDrop<T>,
}

/// A struct for manual reference counting, it is the users responsibility to
/// make sure the reference count remains correct.
///
/// Unlike the Rc from the standart library this pointer will not increment or decrement
/// reference count on its own.
///
/// As such constructing `Rc` from raw pointers is save as using the pointer in any
/// other way the constructing and deconstructing is unsafe.
pub struct Rc<T> {
    ptr: *mut RcVal<T>,
}

impl<T> Clone for Rc<T> {
    fn clone(&self) -> Self {
        Rc { ptr: self.ptr }
    }
}

impl<T> Copy for Rc<T> {}

impl<T> Rc<T> {
    pub fn new(value: T) -> Self {
        unsafe {
            let layout = alloc::Layout::new::<RcVal<T>>();
            let alloc = alloc::alloc(layout) as *mut RcVal<T>;
            alloc.write(RcVal {
                count: 1,
                value: mem::ManuallyDrop::new(value),
            });
            Rc { ptr: alloc }
        }
    }

    #[inline(always)]
    pub fn from_raw(ptr: *mut ()) -> Self {
        Rc { ptr: ptr as _ }
    }

    #[inline(always)]
    pub fn to_raw(self) -> *mut () {
        self.ptr as *mut ()
    }

    #[inline(always)]
    pub unsafe fn incr(&self) {
        (*self.ptr).count += 1;
    }

    #[inline(always)]
    pub unsafe fn value(&self) -> &'static mut T {
        &mut (*self.ptr).value
    }

    #[inline]
    pub unsafe fn drop(self) {
        let val = &mut *self.ptr;
        val.count -= 1;
        if val.count == 0 {
            mem::ManuallyDrop::drop(&mut val.value);
            let layout = alloc::Layout::new::<Rc<T>>();
            std::alloc::dealloc(self.ptr as *mut _, layout)
        }
    }
}

impl<T: Clone + 'static> Rc<T> {
    pub unsafe fn deep_clone(self) -> Self {
        Rc::new(self.value().clone())
    }
}
