use std::cell::UnsafeCell;

use crate::{
    gc::{Gc, Trace},
    realm::RealmCtx,
    value::BoundValue,
    Value,
};
use common::collections::HashMap;

pub struct Object {
    prototype: Option<Gc<Object>>,
    values: UnsafeCell<HashMap<String, Value>>,
    array: UnsafeCell<Vec<Value>>,
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

    pub fn index<'a, K, R>(&self, key: BoundValue<'a>, realm: RealmCtx<'a>) -> R
    where
        K: Into<BoundValue<'a>>,
        R: From<BoundValue<'a>>,
    {
        let key: BoundValue<'a> = key;
        unsafe { BoundValue::bind(self.unsafe_index(key.unbind(), realm)).into() }
    }

    pub fn index_set<'a, K, V>(&self, key: K, value: V, realm: RealmCtx<'a>)
    where
        K: Into<BoundValue<'a>>,
        V: Into<BoundValue<'a>>,
    {
        unsafe {
            self.unsafe_index_set(key.into().unbind(), value.into().unbind(), realm);
        }
    }

    /// Index into the object and return the value assiociated with the key.
    ///
    /// # Safety
    ///
    /// The realm should be the same realm the object was created in.
    pub unsafe fn unsafe_index(&self, key: Value, realm: RealmCtx) -> Value {
        // All uses of unsafe cell are save since no value can hold a reference to
        // an value in the hashmap or vec.
        // And object is not Sync nor Send.
        if key.is_int() {
            let idx = key.cast_int();
            if idx >= 0 {
                return (*self.array.get())
                    .get(idx as usize)
                    .copied()
                    .unwrap_or(Value::undefined());
            }
        }
        let string = realm.coerce_string(BoundValue::bind(key));
        (*self.values.get())
            .get(string.as_ref())
            .copied()
            .unwrap_or(Value::undefined())
    }

    /// Index into the object and set the value assiociated with the key.
    ///
    /// # Safety
    ///
    /// The realm should be the same realm the object was created in.
    pub unsafe fn unsafe_index_set(&self, key: Value, value: Value, realm: RealmCtx) {
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
            let string = realm.coerce_string(BoundValue::bind(key));
            (*self.values.get()).insert(string.to_string(), value);
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
