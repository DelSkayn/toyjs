use std::{
    hash::{Hash, Hasher},
    mem,
};

use crate::{
    gc::{Gc, Trace},
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
    values: HashMap<KeyValue, JSValue>,
}

unsafe impl Trace for Object {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        if let Some(x) = self.prototype.as_ref() {
            x.trace(ctx);
        }
        for (k, v) in self.values.iter() {
            k.trace(ctx);
            v.trace(ctx);
        }
    }
}
