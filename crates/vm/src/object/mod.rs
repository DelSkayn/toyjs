use std::cell::RefCell;

use crate::{
    gc::{Arena, Gc, Rebind, Trace, Tracer},
    rebind,
};

mod elements;
mod function;
mod index;
mod properties;

use elements::Elements;
pub use function::{MutableFn, SharedFn, StaticFn, VmFunction};
use properties::Properties;

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

unsafe impl<'gc, 'cell> Trace for ObjectKind<'gc, 'cell> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, trace: Tracer) {
        match *self {
            ObjectKind::Ordinary
            | ObjectKind::Array
            | ObjectKind::Error
            | ObjectKind::MutableFn(_)
            | ObjectKind::SharedFn(_)
            | ObjectKind::StaticFn(_) => {}
            ObjectKind::VmFn(ref x) => {
                x.trace(trace);
            }
        }
    }
}

unsafe impl<'a, 'gc, 'cell> Rebind<'a> for ObjectKind<'gc, 'cell> {
    type Output = ObjectKind<'a, 'cell>;
}

pub struct Object<'gc, 'cell> {
    prototype: Option<GcObject<'gc, 'cell>>,
    kind: ObjectKind<'gc, 'cell>,
    properties: Properties<'gc, 'cell>,
    elements: Elements<'gc, 'cell>,
}

impl<'gc, 'cell> Object<'gc, 'cell> {
    pub fn new(prototype: Option<GcObject<'gc, 'cell>>, kind: ObjectKind<'gc, 'cell>) -> Self {
        Object {
            prototype,
            kind,
            properties: Properties::new(),
            elements: Elements::new(),
        }
    }

    pub fn new_alloc(
        arena: &'gc Arena<'_, 'cell>,
        prototype: Option<GcObject<'_, 'cell>>,
        kind: ObjectKind<'_, 'cell>,
    ) -> Self {
        let prototype = rebind!(arena, prototype);
        let kind = rebind!(arena, kind);

        Object {
            prototype,
            kind,
            properties: Properties::new(),
            elements: Elements::new(),
        }
    }

    pub fn kind(&self) -> &ObjectKind<'gc, 'cell> {
        &self.kind
    }
}

unsafe impl<'gc, 'cell> Trace for Object<'gc, 'cell> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        false
    }

    fn trace(&self, trace: Tracer) {
        self.prototype.trace(trace);
        self.kind.trace(trace);
        self.properties.trace(trace);
    }
}

unsafe impl<'a, 'gc, 'cell: 'a> Rebind<'a> for Object<'gc, 'cell> {
    type Output = Object<'a, 'cell>;
}
