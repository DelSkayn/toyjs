pub use lock::*;

#[cfg(feature = "parallel")]
mod lock {
    use std::sync::{Arc, Mutex, MutexGuard};

    pub struct Lock<T>(Mutex<T>);

    pub type Ref<T> = Arc<T>;

    pub type Guard<'a, T> = MutexGuard<'a, T>;

    impl<T> Lock<T> {
        pub fn new(v: T) -> Self {
            Lock(Mutex::new(v))
        }

        pub fn lock(&self) -> Guard<T> {
            self.0.lock().unwrap()
        }
    }
}

#[cfg(not(feature = "parallel"))]
mod lock {
    use std::{
        cell::{RefCell, RefMut},
        rc::Rc,
    };

    pub struct Lock<T>(RefCell<T>);

    pub type Ref<T> = Rc<T>;

    pub type Guard<'a, T> = RefMut<'a, T>;

    impl<T> Lock<T> {
        pub fn new(v: T) -> Self {
            Lock(RefCell::new(v))
        }

        pub fn lock(&self) -> Guard<T> {
            self.0.borrow_mut()
        }
    }
}
