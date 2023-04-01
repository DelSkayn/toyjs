use std::sync::{Arc, Mutex, MutexGuard};

pub struct Lock<T>(Arc<Mutex<T>>);

impl<T> Clone for Lock<T> {
    fn clone(&self) -> Self {
        Lock(self.0.clone())
    }
}

pub type Ref<'a, T> = MutexGuard<'a, T>;

impl<T> Lock<T> {
    pub fn new(t: T) -> Self {
        Lock(Arc::new(Mutex::new(t)))
    }

    pub fn lock(&self) -> Ref<T> {
        self.0.lock().unwrap()
    }
}
