use std::{alloc::Layout, ptr::NonNull};

#[repr(C)]
pub struct SmallerVecData<T> {
    len: u32,
    capacity: u32,
}

pub struct SmallerVec<T>(NonNull<SmallerVecData<T>>);

impl<T> SmallerVec<T> {
    const MIN_ALLOC_SIZE: u32 = 64;

    pub fn new() -> Self {
        SmallerVec(NonNull::dangling())
    }

    pub fn len(&self) -> u32 {
        if self.0 == NonNull::dangling() {
            0
        } else {
            unsafe { self.0.as_ref().len }
        }
    }

    pub fn capacity(&self) -> u32 {
        if self.0 == NonNull::dangling() {
            0
        } else {
            unsafe { self.0.as_ref().capacity }
        }
    }

    pub fn is_empty(&self) -> bool {
        self.0 == NonNull::dangling() || unsafe { self.0.as_ref().len == 0 }
    }

    pub fn reserve(&mut self, additional: u32) {
        if additional == 0 {
            return;
        }

        if self.0 == NonNull::dangling() {
            let size = additional
                .next_power_of_two()
                .min(Self::MIN_ALLOC_SIZE / std::mem::size_of::<T>().try_into().unwrap());

            unsafe { self.grow(size) };
        } else {
            let cap = unsafe { self.0.as_ref().capacity };
            let len = unsafe { self.0.as_ref().len };
            let new_len = len.checked_add(additional).unwrap();
            if new_len > cap {
                unsafe { self.grow(new_len.next_power_of_two()) };
            }
        }
    }

    unsafe fn grow(&mut self, to: u32) {
        if self.0 == NonNull::dangling() {
            let data_layout = Layout::array::<T>(to as usize).unwrap();
            let layout = Layout::new::<SmallerVecData<T>>()
                .extend(data_layout)
                .unwrap();
            std::alloc::alloc(layout)
        }
    }

    pub fn push(&mut self, v: T) {}

    pub fn try_push(&mut self, v: T) {}
}

impl<T> Default for SmallerVec<T> {
    fn default() -> Self {
        Self::new()
    }
}
