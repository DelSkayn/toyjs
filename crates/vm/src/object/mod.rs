use core::fmt;

use dreck::{self, Bound, Gc, Root, Trace, Tracer};

//mod elements;
mod function;
use function::{StaticFn, VmFunction};

use self::{elements::Elements, properties::Properties};

mod index;
mod properties;
mod elements;

pub(crate) use properties::{Property, PropertyFlags};

//use elements::Elements;
//pub use function::{FunctionKind, SharedFn, StaticFn, VmFunction};
//use properties::Properties;
//pub use properties::{Accessor, Property, PropertyFlags, PropertyValue};

bitflags::bitflags! {
    pub struct ObjectFlags: u8{
        const EXTENDABLE = 0b1;
        const CONSTRUCTOR = 0b10;

        const ORDINARY = Self::EXTENDABLE.bits;
    }
}

pub type GcObject<'gc, 'own> = Gc<'gc, 'own, Object<'gc, 'own>>;

pub enum ObjectKind<'gc, 'own> {
    Ordinary,
    Array,
    Error,
    VmFn(VmFunction<'gc, 'own>),
    //SharedFn(SharedFn),
    StaticFn(StaticFn),
    Boolean(bool),
    Number(f64),
    String(Gc<'gc, 'own, String>),
    //ForInIterator(ForInIterator),
}

impl<'gc, 'own> fmt::Debug for ObjectKind<'gc, 'own> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "ObjectKind::{}",
            match self {
                Self::Ordinary => "Ordinary",
                Self::Array => "Array",
                Self::Error => "Error",
                Self::VmFn(_) => "VmFn",
                //Self::SharedFn(_) => "SharedFn",
                Self::StaticFn(_) => "StaticFn",
                Self::Boolean(_) => "Boolean",
                Self::Number(_) => "Number",
                Self::String(_) => "String",
                //Self::ForInIterator(_) => "ForInIterator",
            }
        )
    }
}

unsafe impl<'gc, 'own> Trace<'own> for ObjectKind<'gc, 'own> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, trace: Tracer<'a, 'own>) {
        match *self {
            ObjectKind::Ordinary
            | ObjectKind::Array
            | ObjectKind::Error
            //| ObjectKind::SharedFn(_)
            | ObjectKind::StaticFn(_) 
            => {}
            ObjectKind::Boolean(_) => {}
            ObjectKind::Number(_) => {}
            ObjectKind::String(ref x) => {
                x.trace(trace);
            }
            ObjectKind::VmFn(ref x) => {
                x.trace(trace);
            }
        }
    }
}

unsafe impl<'a, 'gc, 'own> Bound<'a> for ObjectKind<'gc, 'own> {
    type Rebound = ObjectKind<'a, 'own>;
}

pub struct Object<'gc, 'own> {
    flags: ObjectFlags,
    prototype: Option<GcObject<'gc, 'own>>,
    kind: ObjectKind<'gc, 'own>,
    pub properties: Properties<'gc, 'own>,
    pub elements: Elements<'gc, 'own>,
}

impl<'gc, 'own> Object<'gc, 'own> {
    pub fn new(
        prototype: Option<GcObject<'gc, 'own>>,
        flags: ObjectFlags,
        kind: ObjectKind<'gc, 'own>,
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
        arena: &'l Root<'own>,
        prototype: Option<GcObject<'_, 'own>>,
        flags: ObjectFlags,
        kind: ObjectKind<'_, 'own>,
    ) -> GcObject<'l, 'own> {
        unsafe {
            arena.add(Object {
                flags,
                prototype: dreck::rebind(prototype),
                kind: dreck::rebind(kind),
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
    pub unsafe fn kind(&self) -> &ObjectKind<'gc, 'own> {
        &self.kind
    }

    pub fn prototype(&self) -> Option<GcObject<'gc, 'own>> {
        self.prototype
    }
}

unsafe impl<'gc, 'own> Trace<'own> for Object<'gc, 'own> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, trace: Tracer<'a, 'own>) {
        self.prototype.trace(trace);
        self.kind.trace(trace);
        self.properties.trace(trace);
        self.elements.trace(trace);
    }
}

unsafe impl<'a, 'gc, 'own: 'a> Bound<'a> for Object<'gc, 'own> {
    type Rebound = Object<'a, 'own>;
}
