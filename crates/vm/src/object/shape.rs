use bitflags::bitflags;
use common::hashmap::HashMap;

use super::props::{PropertyFlags, SlotIdx};
use crate::{
    atom::Atom,
    gc::{self, Free, Gc, RootState, Rooted, Trace},
};

bitflags! {
    #[derive(Clone,Copy,Debug,Eq, PartialEq)]
    pub struct ShapeFlags: u8{
        /// Segment is actively used.
        const DICTIONARY = 0b01;
        /// Segment is part of the nursery for young objects.
        const ARRAY_LIKE = 0b10;
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct Transition {
    atom: Atom,
    flags: PropertyFlags,
}

pub type PropertyMap = HashMap<Atom, SlotIdx>;

pub type GcShape<R> = Gc<R, Shape<R>>;
pub struct Shape<R: RootState> {
    // the parent shape from which we transition into this shape.
    // Can be none when it is the base object.
    parent: Option<GcShape<R>>,

    // The atom which induced this transition
    atom: Atom,
    // The flags of the property transition.
    flags: PropertyFlags,

    // The number of properties within this shape.
    property_count: u8,

    // The transitions from this shape to the next.
    // TODO: Optimize for small number of transitions..
    transitions: HashMap<Transition, GcShape<R>>,

    // Map from atoms to the index within property storage where you can find the property.
    // TODO: Manually implement trace for this map as the atoms don't actually need to be traced.
    property_map: Option<Gc<R, PropertyMap>>,
}

unsafe impl<R: RootState> Trace for Shape<R> {
    type Free<'a> = Shape<Free<'a>>;
    type Rooted = Shape<Rooted>;

    const NEEDS_TRACE: bool = true;

    fn trace(&self, marker: &gc::Marker) -> Result<(), gc::Error> {
        if let Some(parent) = self.parent.as_ref() {
            marker.mark(parent)?;
        }
        marker.mark_atom(self.atom)?;
        for v in self.transitions.values() {
            marker.mark(v)?;
        }
        if let Some(property_map) = self.property_map.as_ref() {
            marker.mark(property_map)?;
        }
        Ok(())
    }
}

impl<R: RootState> Shape<R> {
    pub const MAX_PROPERTIES: u8 = 64;
}
