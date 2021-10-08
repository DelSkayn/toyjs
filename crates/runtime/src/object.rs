use std::{
    cell::UnsafeCell,
    hash::{Hash, Hasher},
    mem,
};

use crate::{
    gc::{Gc, Trace},
    JSValue, Realm,
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
}

impl Object {
    pub fn new() -> Self {
        Object {
            prototype: None,
            values: UnsafeCell::new(HashMap::default()),
        }
    }

    pub unsafe fn index(&self, key: JSValue, realm: &mut Realm) -> JSValue {
        let string = realm.coerce_string(key);
        (*self.values.get())
            .get(&string)
            .copied()
            .unwrap_or(JSValue::undefined())
    }

    pub unsafe fn index_set(&self, key: JSValue, value: JSValue, realm: &mut Realm) {
        let string = realm.coerce_string(key);
        (*self.values.get()).insert(string, value);
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
                //k.trace(ctx);
                v.trace(ctx);
            }
        }
    }
}
