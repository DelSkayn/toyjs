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
#[allow(clippy::module_inception)]
mod lock {
    use std::rc::Rc;

    pub struct Lock<T>(T);

    pub type Ref<T> = Rc<T>;

    pub type Guard<'a, T> = &'a T;

    impl<T> Lock<T> {
        pub fn new(v: T) -> Self {
            Lock(v)
        }

        pub fn lock(&self) -> Guard<T> {
            &self.0
        }
    }
}
