//! Javascript object implementation.

use std::cell::{Cell, UnsafeCell};

use crate::{
    gc::{Gc, Trace},
    Realm, Value,
};
use common::collections::HashMap;

mod function;
pub use function::*;

const ARRAY_BACKDOWN_PROCENT: f64 = 0.7;

bitflags::bitflags! {
    pub struct ObjectFlags: u8{
        const ERROR = 0b1;
        const CONSTRUCTOR = 0b10;
    }
}

#[derive(Debug)]
/// A javascript object
pub struct Object {
    pub prototype: Option<Gc<Object>>,
    values: UnsafeCell<HashMap<String, Value>>,
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
        realm: &Realm,
        prototype: Option<Gc<Object>>,
        flags: ObjectFlags,
    ) -> Gc<Self> {
        realm.vm.allocate(Self::new(prototype, flags))
    }

    #[inline]
    pub unsafe fn alloc_error(realm: &Realm, prototype: Option<Gc<Object>>) -> Gc<Self> {
        realm.vm().allocate(Self::new_error(prototype))
    }

    /// Index into the object and return the value assiociated with the key.
    ///
    /// # Safety
    ///
    /// Value objects must be valid.
    /// The realm should be the same realm the object was created in.
    pub unsafe fn index(&self, key: Value, realm: &Realm) -> Result<Value, Value> {
        // All uses of unsafe cell are save since no value can hold a reference to
        // an value in the hashmap or vec.
        // And object is not Sync nor Send.
        if key.is_int() {
            let idx = key.cast_int();
            if idx >= 0 {
                let idx = idx as usize;
                if self.map_backdown.get().map_or(true, |x| idx < x) {
                    return match (*self.array.get()).get(idx).copied() {
                        Some(x) => Ok(x),
                        None => {
                            if let Some(proto) = self.prototype {
                                proto.index(key, realm)
                            } else {
                                Ok(Value::undefined())
                            }
                        }
                    };
                }
            }
        }
        let string = realm.to_string(key)?;
        match (*self.values.get()).get(&*string).copied() {
            Some(x) => Ok(x),
            None => {
                if let Some(x) = self.prototype {
                    x.index(key, realm)
                } else {
                    Ok(Value::undefined())
                }
            }
        }
    }

    /// Index into the object and set the value assiociated with the key.
    ///
    /// # Safety
    ///
    /// Value objects must be valid.
    /// The realm should be the same realm the object was created in.
    pub unsafe fn index_set(&self, key: Value, value: Value, realm: &Realm) -> Result<(), Value> {
        // All uses of unsafe cell are save since no value can hold a reference to
        // an value in the hashmap or vec.
        // And object is not Sync nor Send.

        if key.is_int() {
            let idx = key.cast_int();
            if idx >= 0 && self.map_backdown.get().map_or(true, |x| (idx as usize) < x) {
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
                            return Ok(());
                        }
                    } else {
                        (*self.array.get()).resize(idx + 1, Value::undefined());
                        (*self.array.get())[idx] = value;
                        return Ok(());
                    }
                } else {
                    (*self.array.get())[idx] = value;
                    return Ok(());
                }
            }
        }
        let string = realm.to_string(key)?;
        (*self.values.get()).insert(string.to_string(), value);
        Ok(())
    }

    /// Set a property of the object directly, skipping possible setters.
    pub unsafe fn raw_index_set(
        &self,
        key: Value,
        value: Value,
        realm: &Realm,
    ) -> Result<(), Value> {
        // All uses of unsafe cell are save since no value can hold a reference to
        // an value in the hashmap or vec.
        // And object is not Sync nor Send.

        if key.is_int() {
            let idx = key.cast_int();
            if idx >= 0 {
                let idx = idx as usize;
                if (*self.array.get()).len() <= idx {
                    (*self.array.get()).resize(idx + 1, Value::undefined());
                }
                (*self.array.get())[idx] = value;
            }
        } else {
            let string = realm.to_string(key)?;
            (*self.values.get()).insert(string.to_string(), value);
        }
        Ok(())
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
}
