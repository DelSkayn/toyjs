//! Javascript object implementation.

use std::cell::UnsafeCell;

use crate::{
    gc::{Gc, Trace},
    Realm, Value,
};
use common::collections::HashMap;

mod function;
pub use function::*;

/// A javascript object
pub struct Object<U: 'static> {
    prototype: Option<Gc<Object<U>>>,
    values: UnsafeCell<HashMap<String, Value>>,
    array: UnsafeCell<Vec<Value>>,
    pub function: Option<FunctionKind<U>>,
}

impl<U: Trace> Object<U> {
    /// Create a new object.
    pub fn new(prototype: Option<Gc<Object<U>>>) -> Self {
        Object {
            prototype,
            values: UnsafeCell::new(HashMap::default()),
            array: UnsafeCell::new(Vec::new()),
            function: None,
        }
    }

    /// Index into the object and return the value assiociated with the key.
    ///
    /// # Safety
    ///
    /// Value objects must be valid.
    /// The realm should be the same realm the object was created in.
    pub unsafe fn index(&self, key: Value, realm: &mut Realm<U>) -> Value {
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
        match (*self.values.get()).get(string.as_ref()).copied() {
            Some(x) => return x,
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
    pub unsafe fn index_set(&self, key: Value, value: Value, realm: &mut Realm<U>) {
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
}

unsafe impl<U: 'static> Trace for Object<U> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        unsafe {
            if let Some(x) = self.prototype.as_ref() {
                x.trace(ctx);
            }
            for (_, v) in (*self.values.get()).iter() {
                v.trace(ctx);
            }
            for v in (*self.array.get()).iter() {
                v.trace(ctx);
            }
        }
    }
}
