use std::cell::UnsafeCell;

use crate::{
    gc::Trace,
    lock::{Guard, Lock, Ref},
    realm::RealmBox,
    Gc, GcArena,
};

pub struct VmInner {
    gc: GcArena,
    realms: UnsafeCell<Vec<RealmBox>>,
}

impl VmInner {
    pub unsafe fn collect_debt<T: Trace>(&self, trace: T) {
        self.gc
            .collect_debt(&((*self.realms.get()).as_slice(), trace))
    }

    pub unsafe fn collect_full<T: Trace>(&self, trace: T) {
        self.gc
            .collect_full(&((*self.realms.get()).as_slice(), trace))
    }

    pub fn allocate<T: Trace + 'static>(&self, v: T) -> Gc<T> {
        self.gc.allocate(v)
    }

    pub fn write_barrier<T: Trace + 'static>(&self, v: Gc<T>) {
        self.gc.write_barrier(v)
    }

    pub fn gc(&self) -> &GcArena {
        &self.gc
    }

    pub fn add_realm(&self, realm: RealmBox) {
        unsafe {
            match (*self.realms.get()).binary_search(&realm) {
                Ok(_) => {}
                Err(e) => {
                    (*self.realms.get()).insert(e, realm);
                }
            }
        }
    }

    pub fn remove_realm(&self, realm: RealmBox) {
        unsafe {
            if let Ok(e) = (*self.realms.get()).binary_search(&realm) {
                (*self.realms.get()).remove(e);
            }
        }
    }
}

impl Drop for VmInner {
    fn drop(&mut self) {
        unsafe { self.gc.collect_all() }
    }
}

#[derive(Clone)]
pub struct Vm(pub(crate) Ref<Lock<VmInner>>);

impl Vm {
    pub fn new() -> Vm {
        Vm(Ref::new(Lock::new(VmInner {
            gc: GcArena::new(),
            realms: UnsafeCell::new(Vec::new()),
        })))
    }

    pub fn collect_full(&self) {
        unsafe { self.0.lock().collect_full(()) }
    }

    pub fn collect_debt(&self) {
        unsafe { self.0.lock().collect_debt(()) }
    }

    pub fn lock(&self) -> Guard<VmInner> {
        self.0.lock()
    }
}
