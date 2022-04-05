//! Javascript object implementation.

use std::cell::{Cell, UnsafeCell};

use crate::{
    atom::Atom,
    gc::{Gc, Trace},
    Realm, Value, VmInner,
};
use common::collections::HashMap;

mod function;
pub use function::*;

const ARRAY_BACKDOWN_PROCENT: f64 = 0.7;

bitflags::bitflags! {
    pub struct ObjectFlags: u8{
        const ERROR = 0b1;
        const CONSTRUCTOR = 0b10;
        const ARRAY= 0b100;
    }
}

#[derive(Debug)]
/// A javascript object
pub struct Object {
    pub prototype: Option<Gc<Object>>,
    values: UnsafeCell<HashMap<Atom, Value>>,
    array: UnsafeCell<Vec<Value>>,
    pub flags: ObjectFlags,
    pub function: Option<FunctionKind>,
    // TODO non null
    map_backdown: Cell<Option<usize>>,
}

impl Object {
    /// Create a new object.
    pub fn new(prototype: Option<Gc<Object>>, flags: ObjectFlags) -> Self {
        Object {
            prototype,
            values: UnsafeCell::new(HashMap::default()),
            array: UnsafeCell::new(Vec::new()),
            map_backdown: Cell::new(None),
            flags,
            function: None,
        }
    }

    #[inline]
    pub fn new_error(prototype: Option<Gc<Object>>) -> Self {
        Self::new(prototype, ObjectFlags::ERROR)
    }

    #[inline]
    pub unsafe fn alloc(
        vm: &VmInner,
        prototype: Option<Gc<Object>>,
        flags: ObjectFlags,
    ) -> Gc<Self> {
        vm.allocate(Self::new(prototype, flags))
    }

    #[inline]
    pub unsafe fn alloc_error(vm: &VmInner, prototype: Option<Gc<Object>>) -> Gc<Self> {
        vm.allocate(Self::new_error(prototype))
    }

    /// Index into the object and return the value assiociated with the key.
    ///
    /// # Safety
    ///
    /// Value objects must be valid.
    /// The realm should be the same realm the object was created in.
    pub unsafe fn index(&self, key: Atom, realm: &Realm) -> Value {
        // All uses of unsafe cell are save since no value can hold a reference to
        // an value in the hashmap or vec.
        // And object is not Sync nor Send.
        if let Some(idx) = key.into_idx() {
            let idx = idx as usize;
            if self.map_backdown.get().map_or(true, |x| idx < x) {
                match (*self.array.get()).get(idx).copied() {
                    Some(x) => return x,
                    None => {
                        return self
                            .prototype
                            .map(|x| x.index(key, realm))
                            .unwrap_or(Value::undefined())
                    }
                };
            }
        }

        (*self.values.get()).get(&key).copied().unwrap_or_else(|| {
            self.prototype
                .map(|x| x.index(key, realm))
                .unwrap_or(Value::undefined())
        })
    }

    /// Index into the object and set the value assiociated with the key.
    ///
    /// # Safety
    ///
    /// Value objects must be valid.
    /// The realm should be the same realm the object was created in.
    pub unsafe fn index_set(&self, key: Atom, value: Value, realm: &Realm) {
        // All uses of unsafe cell are save since no value can hold a reference to
        // an value in the hashmap or vec.
        // And object is not Sync nor Send.

        if let Some(idx) = key.into_idx() {
            if self.map_backdown.get().map_or(true, |x| (idx as usize) < x) {
                let idx = idx as usize;
                let len = (*self.array.get()).len();
                if len <= idx {
                    if (idx - len) as f64 / len as f64 > ARRAY_BACKDOWN_PROCENT {
                        // Fill up to capacity since that part is already allocated any way.
                        let capacity = (*self.array.get()).capacity();
                        (*self.array.get()).resize(capacity, Value::undefined());
                        self.map_backdown.set(Some(capacity));
                        if idx < capacity {
                            (*self.array.get())[idx] = value;
                            return;
                        }
                    } else {
                        (*self.array.get()).resize(idx + 1, Value::undefined());
                        (*self.array.get())[idx] = value;
                        return;
                    }
                } else {
                    (*self.array.get())[idx] = value;
                    return;
                }
            }
        }
        if (*self.values.get()).insert(key, value).is_none() {
            realm.vm().increment(key)
        };
    }

    /// Index into the object and set the value assiociated with the key.
    ///
    /// # Safety
    ///
    /// Value objects must be valid.
    /// The realm should be the same realm the object was created in.
    pub unsafe fn raw_index_set(&self, key: Atom, value: Value, vm: &VmInner) {
        // All uses of unsafe cell are save since no value can hold a reference to
        // an value in the hashmap or vec.
        // And object is not Sync nor Send.

        if let Some(idx) = key.into_idx() {
            if self.map_backdown.get().map_or(true, |x| (idx as usize) < x) {
                let idx = idx as usize;
                let len = (*self.array.get()).len();
                if len <= idx {
                    if (idx - len) as f64 / len as f64 > ARRAY_BACKDOWN_PROCENT {
                        // Fill up to capacity since that part is already allocated any way.
                        let capacity = (*self.array.get()).capacity();
                        (*self.array.get()).resize(capacity, Value::undefined());
                        self.map_backdown.set(Some(capacity));
                        if idx < capacity {
                            (*self.array.get())[idx] = value;
                            return;
                        }
                    } else {
                        (*self.array.get()).resize(idx + 1, Value::undefined());
                        (*self.array.get())[idx] = value;
                        return;
                    }
                } else {
                    (*self.array.get())[idx] = value;
                    return;
                }
            }
        }
        if (*self.values.get()).insert(key, value).is_none() {
            vm.increment(key)
        };
    }

    #[inline]
    pub fn is_error(&self) -> bool {
        self.flags.contains(ObjectFlags::ERROR)
    }

    #[inline]
    pub fn is_constructor(&self) -> bool {
        self.flags.contains(ObjectFlags::CONSTRUCTOR)
    }
}

unsafe impl Trace for Object {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        unsafe {
            if let Some(x) = self.prototype {
                #[cfg(feature = "dump-gc-trace")]
                println!("MARK: obj.proto");
                ctx.mark(x);
            }
            if let Some(x) = self.function.as_ref() {
                x.trace(ctx);
            }
            for v in (*self.values.get()).values() {
                v.trace(ctx);
            }
            for v in &(*self.array.get()) {
                #[cfg(feature = "dump-gc-trace")]
                println!("MARK: obj.entry");
                v.trace(ctx);
            }
        }
    }

    fn finalize(&self, atoms: &crate::atom::Atoms) {
        unsafe {
            (*self.values.get())
                .keys()
                .for_each(|x| atoms.decrement(*x))
        }
    }
}
