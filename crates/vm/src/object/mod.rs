use std::cell::RefCell;

use common::collections::HashMap;

use crate::{
    gc::{Gc, Trace, Tracer},
    value::Value,
};

use self::function::{MutableFn, SharedFn, StaticFn, VmFunction};

mod function;

pub type GcObject<'gc, 'cell> = Gc<'gc, 'cell, Object<'gc, 'cell>>;

pub enum ObjectKind<'gc, 'cell> {
    Ordinary,
    Array,
    Error,
    VmFn(VmFunction<'gc, 'cell>),
    MutableFn(RefCell<MutableFn>),
    SharedFn(SharedFn),
    StaticFn(StaticFn),
    //ForInIterator(ForInIterator),
}

pub struct Object<'gc, 'cell> {
    prototype: Option<GcObject<'gc, 'cell>>,
    kind: ObjectKind<'gc, 'cell>,
    values: HashMap<String, Value<'gc, 'cell>>,
}

impl<'gc, 'cell> Object<'gc, 'cell> {
    pub fn new(prototype: Option<GcObject<'gc, 'cell>>, kind: ObjectKind<'gc, 'cell>) -> Self {
        Object {
            prototype,
            values: HashMap::default(),
            kind,
        }
    }
}

unsafe impl<'gc, 'cell> Trace<'cell> for Object<'gc, 'cell> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        false
    }

    fn trace<'a>(&self, trace: Tracer<'a, 'cell>) {
        self.prototype.trace(trace);
        for v in self.values.values() {
            v.trace(trace);
        }
    }
}
