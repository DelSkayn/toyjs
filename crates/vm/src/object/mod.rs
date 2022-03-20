//! Javascript object implementation.

use std::cell::UnsafeCell;

use crate::{
    gc::{Gc, Trace},
    Realm, Value,
};
use common::collections::HashMap;

mod function;
pub use function::*;

bitflags::bitflags! {
    pub struct ObjectFlags: u8{
        const ERROR = 0b1;
        const CONSTRUCTOR = 0b10;
    }
}

#[derive(Debug)]
/// A javascript object
pub struct Object {
    prototype: Option<Gc<Object>>,
    values: UnsafeCell<HashMap<String, Value>>,
    array: UnsafeCell<Vec<Value>>,
    pub flags: ObjectFlags,
    pub function: Option<FunctionKind>,
}

impl Object {
    /// Create a new object.
    pub fn new(prototype: Option<Gc<Object>>) -> Self {
        Object {
            prototype,
            values: UnsafeCell::new(HashMap::default()),
            array: UnsafeCell::new(Vec::new()),
            flags: ObjectFlags::empty(),
            function: None,
        }
    }

    pub fn new_error(prototype: Option<Gc<Object>>) -> Self {
        Object {
            prototype,
            values: UnsafeCell::new(HashMap::default()),
            array: UnsafeCell::new(Vec::new()),
            flags: ObjectFlags::ERROR,
            function: None,
        }
    }

    /// Index into the object and return the value assiociated with the key.
    ///
    /// # Safety
    ///
    /// Value objects must be valid.
    /// The realm should be the same realm the object was created in.
    pub unsafe fn index(&self, key: Value, realm: &mut Realm) -> Value {
        // All uses of unsafe cell are save since no value can hold a reference to
        // an value in the hashmap or vec.
        // And object is not Sync nor Send.
        if key.is_int() {
            let idx = key.cast_int();
            if idx >= 0 {
                return match (*self.array.get()).get(idx as usize).copied() {
                    Some(x) => x,
                    None => {
                        if let Some(proto) = self.prototype {
                            proto.index(key, realm)
                        } else {
                            Value::undefined()
                        }
                    }
                };
            }
        }
        let string = realm.to_string(key);
        match (*self.values.get()).get(&*string).copied() {
            Some(x) => x,
            None => {
                if let Some(x) = self.prototype {
                    x.index(key, realm)
                } else {
                    Value::undefined()
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
    pub unsafe fn index_set(&self, key: Value, value: Value, realm: &mut Realm) {
        // All uses of unsafe cell are save since no value can hold a reference to
        // an value in the hashmap or vec.
        // And object is not Sync nor Send.

        if key.is_int() {
            let idx = key.cast_int();
            if idx >= 0 {
                let idx = idx as usize;
                if (*self.array.get()).len() <= idx {
                    (*self.array.get()).resize(idx + 1, Value::undefined())
                }
                (*self.array.get())[idx] = value
            }
        } else {
            let string = realm.to_string(key);
            (*self.values.get()).insert(string.to_string(), value);
        }
    }

    /// Set a property of the object directly, skipping possible setters.
    pub unsafe fn raw_index_set(&self, key: Value, value: Value, realm: &mut Realm) {
        // All uses of unsafe cell are save since no value can hold a reference to
        // an value in the hashmap or vec.
        // And object is not Sync nor Send.

        if key.is_int() {
            let idx = key.cast_int();
            if idx >= 0 {
                let idx = idx as usize;
                if (*self.array.get()).len() <= idx {
                    (*self.array.get()).resize(idx + 1, Value::undefined())
                }
                (*self.array.get())[idx] = value
            }
        } else {
            let string = realm.to_string(key);
            (*self.values.get()).insert(string.to_string(), value);
        }
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
                x.trace(ctx)
            }
            for (_k, v) in (*self.values.get()).iter() {
                #[cfg(feature = "dump-gc-trace")]
                println!("MARK: obj.{}", _k);
                v.trace(ctx);
            }
            for v in (*self.array.get()).iter() {
                #[cfg(feature = "dump-gc-trace")]
                println!("MARK: obj.entry");
                v.trace(ctx);
            }
        }
    }
}
