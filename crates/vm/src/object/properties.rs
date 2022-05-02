use core::fmt;
use std::{
    collections::hash_map::{Entry, VacantEntry},
    mem,
    ops::{BitOr, BitOrAssign},
};

use common::collections::HashMap;

use crate::{
    atom::Atom,
    gc::{self, Rebind, Trace, Tracer},
    Value,
};

use super::GcObject;

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct PropertyFlag(u8);

impl PropertyFlag {
    pub const WRITABLE: PropertyFlag = PropertyFlag(0b1);
    pub const ENUMERABLE: PropertyFlag = PropertyFlag(0b10);
    pub const CONFIGURABLE: PropertyFlag = PropertyFlag(0b100);
    const ACCESSOR: PropertyFlag = PropertyFlag(0b1000);

    pub const BUILTIN: PropertyFlag = PropertyFlag(Self::WRITABLE.0 | Self::CONFIGURABLE.0);

    pub const fn empty() -> Self {
        PropertyFlag(0)
    }

    pub const fn ordinary() -> Self {
        PropertyFlag(0b111)
    }

    #[inline]
    pub const fn contains(self, other: Self) -> bool {
        self.0 & other.0 == other.0
    }
}

impl fmt::Debug for PropertyFlag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut start = true;
        if self.contains(PropertyFlag::WRITABLE) {
            write!(f, "WRITABLE")?;
            start = false;
        }
        if self.contains(PropertyFlag::ENUMERABLE) {
            if !start {
                write!(f, " | ")?;
            }
            write!(f, "ENUMERABLE")?;
            start = false;
        }
        if self.contains(PropertyFlag::CONFIGURABLE) {
            if !start {
                write!(f, " | ")?;
            }
            write!(f, "CONFIGURABLE")?;
        }
        Ok(())
    }
}

impl BitOr for PropertyFlag {
    type Output = PropertyFlag;

    fn bitor(self, rhs: Self) -> Self::Output {
        PropertyFlag(self.0 | rhs.0)
    }
}

impl BitOrAssign for PropertyFlag {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

#[derive(Clone, Copy)]
pub struct Accessor<'gc, 'cell> {
    pub get: Option<GcObject<'gc, 'cell>>,
    pub set: Option<GcObject<'gc, 'cell>>,
}

#[derive(Clone, Copy)]
union PropertyUnion<'gc, 'cell> {
    accessor: Accessor<'gc, 'cell>,
    value: Value<'gc, 'cell>,
}

pub enum PropertyValue<'gc, 'cell> {
    Accessor(Accessor<'gc, 'cell>),
    Value(Value<'gc, 'cell>),
}

#[derive(Clone)]
pub struct Property<'gc, 'cell> {
    flags: PropertyFlag,
    atom: Atom,
    value: PropertyUnion<'gc, 'cell>,
}

unsafe impl<'a, 'gc, 'cell> Rebind<'a> for Property<'gc, 'cell> {
    type Output = Property<'a, 'cell>;
}

impl<'gc, 'cell> Property<'gc, 'cell> {
    pub fn accessor(accessor: Accessor<'gc, 'cell>, flags: PropertyFlag, atom: Atom) -> Self {
        Property {
            flags: flags | PropertyFlag::ACCESSOR,
            atom,
            value: PropertyUnion { accessor },
        }
    }

    pub fn value(value: Value<'gc, 'cell>, flags: PropertyFlag, atom: Atom) -> Self {
        Property {
            flags,
            atom,
            value: PropertyUnion { value },
        }
    }

    pub fn is_accessor(&self) -> bool {
        self.flags.contains(PropertyFlag::ACCESSOR)
    }

    pub fn is_enumerable(&self) -> bool {
        self.flags.contains(PropertyFlag::ENUMERABLE)
    }

    pub fn as_value(&self) -> PropertyValue<'gc, 'cell> {
        unsafe {
            if self.flags.contains(PropertyFlag::ACCESSOR) {
                PropertyValue::Accessor(self.value.accessor)
            } else {
                PropertyValue::Value(self.value.value)
            }
        }
    }

    pub fn atom(&self) -> Atom {
        self.atom
    }
}

unsafe impl<'gc, 'cell> Trace for Property<'gc, 'cell> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, trace: Tracer) {
        unsafe {
            if self.flags.contains(PropertyFlag::ACCESSOR) {
                self.value.accessor.get.trace(trace);
                self.value.accessor.set.trace(trace);
            } else {
                self.value.value.trace(trace);
            }
        }
    }
}

#[derive(Clone)]
pub struct Properties<'gc, 'cell> {
    props: Vec<Property<'gc, 'cell>>,
    map: HashMap<Atom, usize>,
}

unsafe impl<'gc, 'cell> Trace for Properties<'gc, 'cell> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, trace: Tracer) {
        self.props.iter().for_each(|x| {
            x.trace(trace);
        })
    }
}

pub struct Vacant<'a, 'gc, 'cell> {
    entry: VacantEntry<'a, Atom, usize>,
    props: &'a mut Vec<Property<'gc, 'cell>>,
}

impl<'a, 'gc, 'cell> Vacant<'a, 'gc, 'cell> {
    pub fn insert(self, value: Value<'_, 'cell>) {
        let idx = self.props.len();
        let key = *self.entry.key();
        // Rebound to the lifetime of props. which are traced so this is safe
        let value = unsafe { gc::rebind(value) };
        self.entry.insert(idx);
        self.props
            .push(Property::value(value, PropertyFlag::ordinary(), key));
    }
}

pub struct Occupied<'a, 'gc, 'cell>(&'a mut Value<'gc, 'cell>);

impl<'a, 'gc, 'cell> Occupied<'a, 'gc, 'cell> {
    pub fn set(&mut self, value: Value<'_, 'cell>) {
        // Safe as the value from the reference will be kept alive for 'gc
        *self.0 = unsafe { gc::rebind(value) };
    }
}

pub enum PropertyEntry<'a, 'gc, 'cell> {
    Occupied(Occupied<'a, 'gc, 'cell>),
    Accessor(GcObject<'gc, 'cell>),
    Vacant(Vacant<'a, 'gc, 'cell>),
    Unwritable,
}

impl<'gc, 'cell> Properties<'gc, 'cell> {
    pub fn new() -> Self {
        Properties {
            props: Vec::new(),
            map: HashMap::default(),
        }
    }

    #[inline]
    pub fn lookup_idx(&self, atom: Atom) -> Option<usize> {
        self.map.get(&atom).copied()
    }

    pub fn clone_properties(&self) -> Vec<Property<'gc, 'cell>> {
        self.props.clone()
    }

    pub fn get(&self, atom: Atom) -> Option<&Property<'gc, 'cell>> {
        self.lookup_idx(atom).and_then(|idx| self.props.get(idx))
    }

    pub fn get_idx(&self, idx: usize) -> Option<&Property<'gc, 'cell>> {
        self.props.get(idx)
    }

    pub fn set_idx(&mut self, idx: usize, property: Property<'gc, 'cell>) {
        self.props[idx] = property;
    }

    pub fn set(&mut self, property: Property<'_, 'cell>) -> Option<Property<'gc, 'cell>> {
        // Safe because property will now be kept alive by properties.
        let property = unsafe { gc::rebind(property) };
        match self.map.entry(property.atom) {
            Entry::Occupied(x) => Some(mem::replace(&mut self.props[*x.get()], property)),
            Entry::Vacant(x) => {
                x.insert(self.props.len());
                self.props.push(property);
                None
            }
        }
    }

    pub fn entry<'a>(&'a mut self, atom: Atom) -> PropertyEntry<'a, 'gc, 'cell> {
        match self.map.entry(atom) {
            Entry::Vacant(entry) => PropertyEntry::Vacant(Vacant {
                entry,
                props: &mut self.props,
            }),
            Entry::Occupied(occ) => unsafe {
                let prop = &mut self.props[*occ.get()];
                if prop.flags.contains(PropertyFlag::WRITABLE) {
                    return PropertyEntry::Unwritable;
                }

                if prop.flags.contains(PropertyFlag::ACCESSOR) {
                    if let Some(getter) = prop.value.accessor.get {
                        PropertyEntry::Accessor(getter)
                    } else {
                        PropertyEntry::Unwritable
                    }
                } else {
                    PropertyEntry::Occupied(Occupied(&mut prop.value.value))
                }
            },
        }
    }
}
