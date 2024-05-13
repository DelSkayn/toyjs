use std::{
    collections::hash_map::{Entry, VacantEntry},
    fmt, mem,
    ops::{BitOr, BitOrAssign},
    slice::Iter,
};

use common::collections::HashMap;
use dreck::{Bound, Trace, Tracer};

use crate::{atom::Atom, value::Value};

use super::GcObject;

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct PropertyFlags(u8);

impl PropertyFlags {
    pub const WRITABLE: PropertyFlags = PropertyFlags(0b1);
    pub const ENUMERABLE: PropertyFlags = PropertyFlags(0b10);
    pub const CONFIGURABLE: PropertyFlags = PropertyFlags(0b100);
    const ACCESSOR: PropertyFlags = PropertyFlags(0b1000);

    pub const BUILTIN: PropertyFlags = PropertyFlags(Self::WRITABLE.0 | Self::CONFIGURABLE.0);

    pub const fn empty() -> Self {
        PropertyFlags(0)
    }

    pub const fn ordinary() -> Self {
        PropertyFlags(0b111)
    }

    #[inline]
    pub const fn contains(self, other: Self) -> bool {
        self.0 & other.0 == other.0
    }

    #[inline]
    pub const fn clear(self, other: Self) -> Self {
        Self(self.0 & !other.0)
    }
}

impl fmt::Debug for PropertyFlags {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut start = true;
        if self.contains(PropertyFlags::WRITABLE) {
            write!(f, "WRITABLE")?;
            start = false;
        }
        if self.contains(PropertyFlags::ENUMERABLE) {
            if !start {
                write!(f, " | ")?;
            }
            write!(f, "ENUMERABLE")?;
            start = false;
        }
        if self.contains(PropertyFlags::CONFIGURABLE) {
            if !start {
                write!(f, " | ")?;
            }
            write!(f, "CONFIGURABLE")?;
        }
        Ok(())
    }
}

impl BitOr for PropertyFlags {
    type Output = PropertyFlags;

    fn bitor(self, rhs: Self) -> Self::Output {
        PropertyFlags(self.0 | rhs.0)
    }
}

impl BitOrAssign for PropertyFlags {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

#[derive(Clone, Copy)]
pub struct Accessor<'gc, 'own> {
    pub get: Option<GcObject<'gc, 'own>>,
    pub set: Option<GcObject<'gc, 'own>>,
}

#[derive(Clone, Copy)]
union PropertyUnion<'gc, 'own> {
    accessor: Accessor<'gc, 'own>,
    value: Value<'gc, 'own>,
}

pub enum PropertyValue<'gc, 'own> {
    Accessor(Accessor<'gc, 'own>),
    Value(Value<'gc, 'own>),
}

#[derive(Clone)]
pub struct Property<'gc, 'own> {
    flags: PropertyFlags,
    atom: Atom<'gc, 'own>,
    value: PropertyUnion<'gc, 'own>,
}

impl<'gc, 'own> fmt::Debug for Property<'gc, 'own> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            if self.flags.contains(PropertyFlags::ACCESSOR) {
                f.debug_struct("Property::Accessor")
                    //.field("value", &self.value.accessor)
                    .field("flags", &self.flags)
                    .field("atom", &self.atom)
                    .finish()
            } else {
                f.debug_struct("Property::Ordinary")
                    .field("value", &self.value.value)
                    .field("flags", &self.flags)
                    .field("atom", &self.atom)
                    .finish()
            }
        }
    }
}

unsafe impl<'a, 'gc, 'own> Bound<'a> for Property<'gc, 'own> {
    type Rebound = Property<'a, 'own>;
}

unsafe impl<'own> Trace<'own> for Property<'_, 'own> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, tracer: dreck::Tracer<'a, 'own>) {
        if self.flags.contains(PropertyFlags::ACCESSOR) {
            let v = unsafe { &self.value.accessor };
            v.get.trace(tracer);
            v.set.trace(tracer);
        } else {
            unsafe {
                self.value.value.trace(tracer);
            }
        }
    }
}

impl<'gc, 'own> Property<'gc, 'own> {
    pub fn accessor(
        accessor: Accessor<'gc, 'own>,
        flags: PropertyFlags,
        atom: Atom<'gc, 'own>,
    ) -> Self {
        Property {
            flags: flags | PropertyFlags::ACCESSOR,
            atom,
            value: PropertyUnion { accessor },
        }
    }

    pub fn value(value: Value<'gc, 'own>, flags: PropertyFlags, atom: Atom<'gc, 'own>) -> Self {
        Property {
            flags,
            atom,
            value: PropertyUnion { value },
        }
    }

    pub fn is_accessor(&self) -> bool {
        self.flags.contains(PropertyFlags::ACCESSOR)
    }

    pub fn is_enumerable(&self) -> bool {
        self.flags.contains(PropertyFlags::ENUMERABLE)
    }

    pub fn as_value(&self) -> PropertyValue<'gc, 'own> {
        unsafe {
            if self.flags.contains(PropertyFlags::ACCESSOR) {
                PropertyValue::Accessor(self.value.accessor)
            } else {
                PropertyValue::Value(self.value.value)
            }
        }
    }

    pub fn atom(&self) -> Atom<'gc, 'own> {
        self.atom
    }
}

#[derive(Clone)]
pub struct Properties<'gc, 'own> {
    props: Vec<Property<'gc, 'own>>,
    map: HashMap<Atom<'gc, 'own>, usize>,
}

unsafe impl<'gc, 'own> Trace<'own> for Properties<'gc, 'own> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, trace: Tracer<'a, 'own>) {
        self.props.iter().for_each(|x| {
            x.trace(trace);
        })
    }
}

#[derive(Debug)]
pub struct Vacant<'a, 'gc, 'own> {
    entry: VacantEntry<'a, Atom<'gc, 'own>, usize>,
    props: &'a mut Vec<Property<'gc, 'own>>,
}

impl<'a, 'gc, 'own> Vacant<'a, 'gc, 'own> {
    pub fn insert(self, value: Value<'_, 'own>) {
        let idx = self.props.len();
        let key = *self.entry.key();
        // Rebound to the lifetime of props. which are traced so this is safe
        let value = unsafe { dreck::rebind(value) };
        self.entry.insert(idx);
        self.props
            .push(Property::value(value, PropertyFlags::ordinary(), key));
    }
}

#[derive(Debug)]
pub struct Occupied<'a, 'gc, 'own>(&'a mut Value<'gc, 'own>);

impl<'a, 'gc, 'own> Occupied<'a, 'gc, 'own> {
    pub fn set(&mut self, value: Value<'_, 'own>) {
        // Safe as the value from the reference will be kept alive for 'gc
        *self.0 = unsafe { dreck::rebind(value) };
    }
}

//#[derive(Debug)]
pub enum PropertyEntry<'a, 'gc, 'own> {
    Occupied(Occupied<'a, 'gc, 'own>),
    Accessor(GcObject<'gc, 'own>),
    Vacant(Vacant<'a, 'gc, 'own>),
    Unwritable,
}

impl<'gc, 'own> Properties<'gc, 'own> {
    pub fn new() -> Self {
        Properties {
            props: Vec::new(),
            map: HashMap::default(),
        }
    }

    #[inline]
    pub fn lookup_idx(&self, atom: Atom<'_, 'own>) -> Option<usize> {
        self.map.get(&atom).copied()
    }

    pub fn clone_properties(&self) -> Vec<Property<'gc, 'own>> {
        self.props.clone()
    }

    pub fn get(&self, atom: Atom<'_, 'own>) -> Option<&Property<'gc, 'own>> {
        self.lookup_idx(atom).and_then(|idx| self.props.get(idx))
    }

    pub fn get_idx(&self, idx: usize) -> Option<&Property<'gc, 'own>> {
        self.props.get(idx)
    }

    pub fn set_idx(&mut self, idx: usize, property: Property<'gc, 'own>) {
        self.props[idx] = property;
    }

    pub fn set(&mut self, property: Property<'_, 'own>) -> Option<Property<'gc, 'own>> {
        // Safe because property will now be kept alive by properties.
        let property = unsafe { dreck::rebind(property) };
        match self.map.entry(property.atom) {
            Entry::Occupied(x) => Some(mem::replace(&mut self.props[*x.get()], property)),
            Entry::Vacant(x) => {
                x.insert(self.props.len());
                self.props.push(property);
                None
            }
        }
    }

    pub fn freeze(&mut self) {
        self.props.iter_mut().for_each(|x| {
            x.flags = x
                .flags
                .clear(PropertyFlags::WRITABLE | PropertyFlags::CONFIGURABLE)
        })
    }

    pub fn is_frozen(&self) -> bool {
        self.props.iter().all(|x| {
            !(x.flags.contains(PropertyFlags::WRITABLE)
                || x.flags.contains(PropertyFlags::CONFIGURABLE))
        })
    }

    pub fn seal(&mut self) {
        self.props
            .iter_mut()
            .for_each(|x| x.flags = x.flags.clear(PropertyFlags::CONFIGURABLE))
    }

    pub fn is_sealed(&self) -> bool {
        self.props
            .iter()
            .all(|x| !x.flags.contains(PropertyFlags::CONFIGURABLE))
    }

    pub fn entry<'a>(&'a mut self, atom: Atom<'_, 'own>) -> PropertyEntry<'a, 'gc, 'own> {
        match self.map.entry(unsafe { dreck::rebind(atom) }) {
            Entry::Vacant(entry) => PropertyEntry::Vacant(Vacant {
                entry,
                props: &mut self.props,
            }),
            Entry::Occupied(occ) => unsafe {
                let prop = &mut self.props[*occ.get()];
                if prop.flags.contains(PropertyFlags::ACCESSOR) {
                    if let Some(setter) = prop.value.accessor.set {
                        PropertyEntry::Accessor(setter)
                    } else {
                        PropertyEntry::Unwritable
                    }
                } else {
                    if !prop.flags.contains(PropertyFlags::WRITABLE) {
                        return PropertyEntry::Unwritable;
                    }
                    PropertyEntry::Occupied(Occupied(&mut prop.value.value))
                }
            },
        }
    }

    pub fn iter(&self) -> Iter<Property<'gc, 'own>> {
        self.props.iter()
    }
}
