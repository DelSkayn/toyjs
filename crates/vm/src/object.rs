use std::cell::UnsafeCell;

use crate::{
    gc::{Gc, Trace},
    realm::Realm,
    JSValue,
};
use common::collections::HashMap;

pub struct Object {
    prototype: Option<Gc<Object>>,
    values: UnsafeCell<HashMap<String, JSValue>>,
    array: UnsafeCell<Vec<JSValue>>,
}

impl Object {
    /// Create a new object.
    pub fn new() -> Self {
        Object {
            prototype: None,
            values: UnsafeCell::new(HashMap::default()),
            array: UnsafeCell::new(Vec::new()),
        }
    }

    /// Index into the object and return the value assiociated with the key.
    ///
    /// # Safety
    ///
    /// The realm should be the same realm the object was created in.
    pub unsafe fn index(&self, key: JSValue, realm: &mut Realm) -> JSValue {
        // All uses of unsafe cell are save since no value can hold a reference to
        // an value in the hashmap or vec.
        // And object is not Sync nor Send.
        //
        if key.is_int() {
            let idx = key.into_int();
            if idx >= 0 {
                return (*self.array.get())
                    .get(idx as usize)
                    .copied()
                    .unwrap_or(JSValue::undefined());
            }
        }
        let string = realm.coerce_string(key);
        (*self.values.get())
            .get(&string)
            .copied()
            .unwrap_or(JSValue::undefined())
    }

    /// Index into the object and set the value assiociated with the key.
    ///
    /// # Safety
    ///
    /// The realm should be the same realm the object was created in.
    pub unsafe fn index_set(&self, key: JSValue, value: JSValue, realm: &mut Realm) {
        // All uses of unsafe cell are save since no value can hold a reference to
        // an value in the hashmap or vec.
        // And object is not Sync nor Send.

        if key.is_int() {
            let idx = key.into_int();
            if idx >= 0 {
                let idx = idx as usize;
                if (*self.array.get()).len() <= idx {
                    (*self.array.get()).resize(idx + 1, JSValue::undefined())
                }
                return (*self.array.get())[idx] = value;
            }
        } else {
            let string = realm.coerce_string(key);
            (*self.values.get()).insert(string, value);
        }
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
