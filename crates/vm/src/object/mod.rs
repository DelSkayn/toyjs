use core::fmt;

use crate::gc::{self, Arena, Gc, Rebind, Trace, Tracer};

mod elements;
mod function;
mod index;
mod properties;

use elements::Elements;
pub use function::{FunctionKind, SharedFn, StaticFn, VmFunction};
use properties::Properties;
pub use properties::{Accessor, Property, PropertyFlags, PropertyValue};

bitflags::bitflags! {
    pub struct ObjectFlags: u8{
        const EXTENDABLE = 0b1;
        const CONSTRUCTOR = 0b10;

        const ORDINARY = Self::EXTENDABLE.bits;
    }
}

pub type GcObject<'gc, 'cell> = Gc<'gc, 'cell, Object<'gc, 'cell>>;

pub enum ObjectKind<'gc, 'cell> {
    Ordinary,
    Array,
    Error,
    VmFn(VmFunction<'gc, 'cell>),
    SharedFn(SharedFn),
    StaticFn(StaticFn),
    //ForInIterator(ForInIterator),
}

impl<'gc, 'cell> fmt::Debug for ObjectKind<'gc, 'cell> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "ObjectKind::{}",
            match self {
                Self::Ordinary => "Ordinary",
                Self::Array => "Array",
                Self::Error => "Error",
                Self::VmFn(_) => "VmFn",
                Self::SharedFn(_) => "SharedFn",
                Self::StaticFn(_) => "StaticFn",
                //Self::ForInIterator(_) => "ForInIterator",
            }
        )
    }
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
    flags: ObjectFlags,
    prototype: Option<GcObject<'gc, 'cell>>,
    kind: ObjectKind<'gc, 'cell>,
    pub(crate) properties: Properties<'gc, 'cell>,
    pub(crate) elements: Elements<'gc, 'cell>,
}

impl<'gc, 'cell> Object<'gc, 'cell> {
    pub fn new(
        prototype: Option<GcObject<'gc, 'cell>>,
        flags: ObjectFlags,
        kind: ObjectKind<'gc, 'cell>,
    ) -> Self {
        Object {
            flags,
            prototype,
            kind,
            properties: Properties::new(),
            elements: Elements::new(),
        }
    }

    pub fn new_gc<'l>(
        arena: &'l Arena<'_, 'cell>,
        prototype: Option<GcObject<'_, 'cell>>,
        flags: ObjectFlags,
        kind: ObjectKind<'_, 'cell>,
    ) -> GcObject<'l, 'cell> {
        unsafe {
            arena.add(Object {
                flags,
                prototype: gc::rebind(prototype),
                kind: gc::rebind(kind),
                properties: Properties::new(),
                elements: Elements::new(),
            })
        }
    }

    pub fn flags(&self) -> ObjectFlags {
        self.flags
    }

    /// # Safety
    /// Accessing function kinds as mutable is unsafe
    pub unsafe fn kind(&self) -> &ObjectKind<'gc, 'cell> {
        &self.kind
    }

    pub fn prototype(&self) -> Option<GcObject<'gc, 'cell>> {
        self.prototype
    }
}

unsafe impl<'gc, 'cell> Trace for Object<'gc, 'cell> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, trace: Tracer) {
        self.prototype.trace(trace);
        self.kind.trace(trace);
        self.properties.trace(trace);
        self.elements.trace(trace);
    }

    fn finalize(&self, atoms: &common::atom::Atoms) {
        for p in self.properties.iter() {
            atoms.decrement(p.atom());
        }
    }
}

unsafe impl<'a, 'gc, 'cell: 'a> Rebind<'a> for Object<'gc, 'cell> {
    type Output = Object<'a, 'cell>;
}
