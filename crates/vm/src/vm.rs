use std::cell::UnsafeCell;

use crate::{
    atom::{Atom, Atoms},
    gc::Trace,
    lock::{Guard, Lock, Ref},
    realm::RealmBox,
    Gc, GcArena, Realm, Value,
};

pub struct VmInner {
    atoms: Atoms,
    gc: GcArena,
    realms: UnsafeCell<Vec<RealmBox>>,
}

impl VmInner {
    pub unsafe fn collect_debt<T: Trace>(&self, trace: T) {
        self.gc
            .collect_debt(&((*self.realms.get()).as_slice(), trace), &self.atoms);
    }

    pub unsafe fn collect_full<T: Trace>(&self, trace: T) {
        self.gc
            .collect_full(&((*self.realms.get()).as_slice(), trace), &self.atoms);
    }

    pub fn allocate<T: Trace + 'static>(&self, v: T) -> Gc<T> {
        self.gc.allocate(v)
    }

    pub fn write_barrier<T: Trace + 'static>(&self, v: Gc<T>) {
        self.gc.write_barrier(v);
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

    pub fn atoms(&self) -> &Atoms {
        &self.atoms
    }

    pub unsafe fn atomize(&self, value: Value, realm: &Realm) -> Result<Atom, Value> {
        self.atoms.atomize(value, realm)
    }

    pub fn atomize_string(&self, text: &str) -> Atom {
        self.atoms.atomize_string(text)
    }

    pub fn increment(&self, atom: Atom) {
        self.atoms.increment(atom)
    }

    pub fn decrement(&self, atom: Atom) {
        self.atoms.decrement(atom)
    }
}

impl Drop for VmInner {
    fn drop(&mut self) {
        unsafe { self.gc.collect_all(&self.atoms) }
    }
}

#[derive(Clone)]
pub struct Vm(pub(crate) Ref<Lock<VmInner>>);

impl Vm {
    pub fn new() -> Vm {
        Vm(Ref::new(Lock::new(VmInner {
            atoms: Atoms::new(),
            gc: GcArena::new(),
            realms: UnsafeCell::new(Vec::new()),
        })))
    }

    pub fn collect_full(&self) {
        unsafe { self.0.lock().collect_full(()) };
    }

    pub fn collect_debt(&self) {
        unsafe { self.0.lock().collect_debt(()) };
    }

    pub fn lock(&self) -> Guard<VmInner> {
        self.0.lock()
    }
}
