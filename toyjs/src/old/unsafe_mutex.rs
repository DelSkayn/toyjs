use std::{
    cell::UnsafeCell,
    ops::{Deref, DerefMut},
    sync::{LockResult, Mutex, MutexGuard, PoisonError},
};

pub struct UnsafeMutex<T> {
    lock: Mutex<()>,
    data: UnsafeCell<T>,
}

unsafe impl<T> Sync for UnsafeMutex<T> {}
unsafe impl<T> Send for UnsafeMutex<T> {}

impl<T> UnsafeMutex<T> {
    pub fn new(t: T) -> Self {
        UnsafeMutex {
            lock: Mutex::new(()),
            data: UnsafeCell::new(t),
        }
    }

    pub fn lock<'a>(&'a self) -> LockResult<UnsafeMutexGuard<'a, T>> {
        match self.lock.lock() {
            Ok(x) => Ok(UnsafeMutexGuard {
                guard: x,
                value: unsafe { &mut (*self.data.get()) },
            }),
            Err(e) => Err(PoisonError::new(UnsafeMutexGuard {
                guard: e.into_inner(),
                value: unsafe { &mut (*self.data.get()) },
            })),
        }
    }

    pub unsafe fn bypass(&self) -> &mut T {
        &mut (*self.data.get())
    }
}

pub struct UnsafeMutexGuard<'a, T> {
    guard: MutexGuard<'a, ()>,
    value: &'a mut T,
}

impl<'a, T> Deref for UnsafeMutexGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.value
    }
}

impl<'a, T> DerefMut for UnsafeMutexGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.value
    }
}
