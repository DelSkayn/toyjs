use std::{
    cell::UnsafeCell,
    hash::{Hash, Hasher},
    mem,
};

use crate::{
    gc::{Gc, Trace},
    realm::Realm,
    JSValue,
};
use common::collections::HashMap;

#[derive(Debug, Eq, PartialEq)]
pub enum KeyValue {
    Integer(i32),
    String(Gc<String>),
}

unsafe impl Trace for KeyValue {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        match *self {
            KeyValue::String(x) => ctx.mark(x),
            KeyValue::Integer(_) => {}
        }
    }
}

impl Hash for KeyValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match *self {
            Self::Integer(x) => x.hash(state),
            KeyValue::String(x) => x.hash(state),
        }
    }
}

pub struct Object {
    prototype: Option<Gc<Object>>,
    values: UnsafeCell<HashMap<String, JSValue>>,
    array: UnsafeCell<Vec<JSValue>>,
}

impl Object {
    pub fn new() -> Self {
        Object {
            prototype: None,
            values: UnsafeCell::new(HashMap::default()),
            array: UnsafeCell::new(Vec::new()),
        }
    }

    pub unsafe fn index(&self, key: JSValue, realm: &mut Realm) -> JSValue {
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

    pub unsafe fn index_set(&self, key: JSValue, value: JSValue, realm: &mut Realm) {
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
