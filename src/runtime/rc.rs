use std::{alloc, mem};

#[repr(C)]
pub struct RcCount {
    count: u64,
}

impl RcCount {
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

/// Struct for manual reference counting, it is the users responsibility to
/// keep the count correct.
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
            (*alloc).count = 1;
            (*alloc).value = mem::ManuallyDrop::new(value);
            Rc { ptr: alloc }
        }
    }

    #[inline(always)]
    pub unsafe fn from_raw(ptr: *mut ()) -> Self {
        Rc { ptr: ptr as _ }
    }

    #[inline(always)]
    pub fn to_raw(self) -> *mut () {
        self.ptr as *mut ()
    }

    #[inline(always)]
    pub fn incr(&mut self) {
        unsafe {
            (*self.ptr).count += 1;
        }
    }

    #[inline(always)]
    pub unsafe fn value(self) -> &'static T {
        &(*self.ptr).value
    }

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
    pub unsafe fn clone_obj(self) -> Self {
        Rc::new(self.value().clone())
    }
}
