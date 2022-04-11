use std::{cell::RefCell, fmt};

use crate::{gc::Trace, Gc, VmInner};

mod function;
pub use function::{MutableFn, SharedFn, StaticFn, VmFunction, RECURSIVE_FUNC_PANIC};
mod property;
use property::Properties;
pub use property::{Accessor, GetResult, Property, PropertyFlags, PropertyValue, SetResult};
mod elements;
use elements::Elements;

mod index;
mod iterator;
pub use iterator::ForInIterator;

bitflags::bitflags! {
    pub struct ObjectFlags: u8{
        const EXTENABLE = 0b1;
        const CONSTRUCTOR = 0b10;
        const ORDINARY = Self::EXTENABLE.bits;
    }
}

pub enum ObjectKind {
    Ordinary,
    Array,
    Error,
    VmFn(VmFunction),
    MutableFn(RefCell<MutableFn>),
    SharedFn(SharedFn),
    StaticFn(StaticFn),
    ForInIterator(ForInIterator),
}

unsafe impl Trace for ObjectKind {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        match *self {
            Self::Ordinary
            | Self::Array
            | Self::Error
            | Self::MutableFn(_)
            | Self::SharedFn(_)
            | Self::StaticFn(_) => {}
            Self::VmFn(ref x) => {
                x.trace(ctx);
            }
            Self::ForInIterator(ref x) => {
                x.trace(ctx);
            }
        }
    }
}

impl fmt::Debug for ObjectKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "ObjectKind::{}",
            match self {
                Self::Ordinary => "Ordinary",
                Self::Array => "Array",
                Self::Error => "Error",
                Self::VmFn(_) => "VmFn",
                Self::MutableFn(_) => "MutableFn",
                Self::SharedFn(_) => "SharedFn",
                Self::StaticFn(_) => "StaticFn",
                Self::ForInIterator(_) => "ForInIterator",
            }
        )
    }
}

#[derive(Debug)]
pub struct Object {
    pub prototype: Option<Gc<Object>>,
    pub flags: ObjectFlags,

    pub(crate) properties: Properties,
    pub(crate) elements: Elements,

    pub kind: ObjectKind,
}

unsafe impl Trace for Object {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        self.prototype.trace(ctx);
        self.kind.trace(ctx);
        self.elements.trace(ctx);
        self.properties.trace(ctx)
    }

    fn finalize(&self, atoms: &crate::atom::Atoms) {
        self.properties.finalize(atoms);
    }
}

impl Object {
    pub fn new(prototype: Option<Gc<Object>>, flags: ObjectFlags, kind: ObjectKind) -> Self {
        Object {
            prototype,
            flags,
            kind,

            properties: Properties::new(),
            elements: Elements::new(),
        }
    }

    pub fn new_gc(
        vm: &VmInner,
        prototype: Option<Gc<Object>>,
        flags: ObjectFlags,
        kind: ObjectKind,
    ) -> Gc<Self> {
        vm.allocate(Self::new(prototype, flags, kind))
    }
}
