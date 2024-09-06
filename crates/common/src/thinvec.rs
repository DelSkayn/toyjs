use std::{
    alloc::{self, Layout},
    io::Write,
    marker::PhantomData,
    mem::{self, offset_of},
    ops::Deref,
    ptr::{self, NonNull},
};

#[repr(C)]
struct Header {
    capacity: u32,
    len: u32,
}

static EMPTY: Header = Header {
    capacity: 0,
    len: 0,
};

#[repr(C)]
struct HeaderValue<T> {
    header: Header,
    value: [T; 2],
}

pub struct ThinVec<T> {
    ptr: NonNull<Header>,
    _marker: PhantomData<T>,
}

impl<T> ThinVec<T> {
    const MIN_ALLOC_SIZE: usize = if mem::size_of::<T>() == 1 {
        8
    } else if mem::size_of::<T>() <= 1024 {
        4
    } else {
        1
    };

    pub fn new() -> Self {
        ThinVec {
            ptr: NonNull::from(&EMPTY),
            _marker: PhantomData,
        }
    }

    pub fn push(&mut self, value: T) {
        let len = self.len();
        std::io::stdout().flush().unwrap();
        if len == u32::MAX {
            panic!("exceeded maximum size");
        }

        if self.capacity() - len == 0 {
            self.grow_one()
        }

        unsafe {
            self.data_ptr().add(len as usize).write(value);
        }

        unsafe { self.ptr.as_mut().len = len + 1 };
    }

    pub fn get(&self, index: u32) -> Option<&T> {
        if self.len() <= index {
            return None;
        }
        unsafe { Some(&(*self.data_ptr().add(index as usize))) }
    }

    pub fn get_mut(&mut self, index: u32) -> Option<&mut T> {
        if self.len() <= index {
            return None;
        }
        unsafe { Some(&mut (*self.data_ptr().add(index as usize))) }
    }

    pub fn clear(&mut self) {
        if self.len() == 0 {
            return;
        }

        if mem::needs_drop::<T>() {
            let data = self.data_ptr();
            for i in 0..self.len() {
                unsafe {
                    ptr::drop_in_place(data.add(i as usize));
                }
            }
        }
        unsafe {
            self.ptr.as_mut().len = 0;
        }
    }

    pub fn len(&self) -> u32 {
        unsafe { self.ptr.as_ref().len }
    }

    pub fn is_empty(&self) -> bool {
        unsafe { self.ptr.as_ref().len == 0 }
    }

    pub fn capacity(&self) -> u32 {
        unsafe { self.ptr.as_ref().capacity }
    }

    pub fn as_slice(&self) -> &[T] {
        unsafe {
            let len = self.ptr.as_ref().len;
            let data = self.data_ptr();
            std::slice::from_raw_parts(data, len as usize)
        }
    }

    pub fn as_slice_mut(&mut self) -> &mut [T] {
        unsafe {
            let len = self.ptr.as_ref().len;
            let data = self.data_ptr();

            std::slice::from_raw_parts_mut(data, len as usize)
        }
    }

    fn data_ptr(&self) -> *mut T {
        unsafe {
            self.ptr
                .as_ptr()
                .byte_add(offset_of!(HeaderValue<T>, value))
                .cast::<T>()
        }
    }

    #[cold]
    fn grow_one(&mut self) {
        let current_capacity = self.capacity();
        if current_capacity == 0 {
            let (layout, offset) = Layout::new::<Header>()
                .extend(Layout::array::<T>(Self::MIN_ALLOC_SIZE).unwrap())
                .unwrap();

            debug_assert_eq!(offset, offset_of!(HeaderValue<T>, value));

            let memory = unsafe { alloc::alloc(layout) };
            self.ptr = NonNull::new(memory)
                .expect("failed to allocate")
                .cast::<Header>();
            unsafe {
                self.ptr.write(Header {
                    capacity: Self::MIN_ALLOC_SIZE as u32,
                    len: 0,
                });
            }
        } else {
            let (layout, _) = Layout::new::<Header>()
                .extend(Layout::array::<T>(current_capacity as usize).unwrap())
                .unwrap();

            let new_capacity = (current_capacity + 1).next_power_of_two();

            let new_size = Layout::new::<Header>()
                .extend(Layout::array::<T>(new_capacity as usize).unwrap())
                .unwrap()
                .0
                .size();

            let memory = unsafe { alloc::realloc(self.ptr.as_ptr().cast(), layout, new_size) };
            self.ptr = NonNull::new(memory)
                .expect("failed to allocate")
                .cast::<Header>();

            unsafe { self.ptr.as_mut().capacity = new_capacity };
        }
    }
}

impl<T> Default for ThinVec<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Drop for ThinVec<T> {
    fn drop(&mut self) {
        self.clear();

        let capacity = self.capacity();
        if capacity != 0 {
            let (layout, _) = Layout::new::<Header>()
                .extend(Layout::array::<T>(capacity as usize).unwrap())
                .unwrap();
            unsafe { alloc::dealloc(self.ptr.as_ptr().cast(), layout) };
        }
    }
}

impl<T> Deref for ThinVec<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

#[cfg(test)]
mod test {
    use std::cell::Cell;

    use super::ThinVec;

    #[test]
    pub fn basic_usage() {
        let mut vec = ThinVec::new();

        vec.push(1);
        vec.push(2);
        vec.push(3);
        vec.push(4);

        assert_eq!(vec[0], 1);
        assert_eq!(vec[1], 2);
        assert_eq!(vec[2], 3);
        assert_eq!(vec[3], 4);

        vec.clear();

        for i in 0..100 {
            vec.push(i);
        }

        for i in 0..100 {
            assert_eq!(vec[i], i);
        }
    }

    #[test]
    pub fn drops_correctly() {
        thread_local! {
            pub static TEST: Cell<usize> = Cell::new(0);
        }

        struct Dropper;

        impl Drop for Dropper {
            fn drop(&mut self) {
                TEST.with(|x| x.set(x.get() + 1));
            }
        }

        {
            let mut vec = ThinVec::new();

            vec.push(Dropper);
            vec.push(Dropper);
        }
        assert_eq!(TEST.with(|x| x.get()), 2);

        let mut vec = ThinVec::new();

        vec.push(Dropper);
        vec.push(Dropper);

        vec.clear();
        assert_eq!(TEST.with(|x| x.get()), 4);
    }
}
