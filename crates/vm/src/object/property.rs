use std::{cell::UnsafeCell, collections::hash_map::Entry, fmt, ops::BitOr};

use common::{cell_vec::CellVec, collections::HashMap};

use crate::{atom::Atom, gc::Trace, Gc, Object, Value};

#[derive(Clone, Copy)]
pub struct PropertyFlags(u8);

impl PropertyFlags {
    pub const WRITABLE: PropertyFlags = PropertyFlags(0b1);
    pub const ENUMERABLE: PropertyFlags = PropertyFlags(0b10);
    pub const CONFIGURABLE: PropertyFlags = PropertyFlags(0b100);

    pub const ORDINARY: PropertyFlags =
        PropertyFlags(Self::WRITABLE.0 | Self::ENUMERABLE.0 | Self::CONFIGURABLE.0);

    const ACCESSOR: PropertyFlags = PropertyFlags(0b1000);

    #[inline]
    pub fn contains(self, flag: PropertyFlags) -> bool {
        (self.0 & flag.0) == flag.0
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
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Accessor {
    get: Option<Gc<Object>>,
    set: Option<Gc<Object>>,
}

pub enum PropertyValue {
    Accessor(Accessor),
    Value(Value),
}

#[derive(Clone, Copy)]
union PropertyUnion {
    accessor: Accessor,
    value: Value,
}

#[derive(Clone, Copy)]
pub struct Property {
    pub flags: PropertyFlags,
    pub key: Atom,
    value: PropertyUnion,
}

impl fmt::Debug for Property {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            if self.flags.contains(PropertyFlags::ACCESSOR) {
                f.debug_struct("Property::Accessor")
                    .field("value", &self.value.accessor)
                    .field("flags", &self.flags)
                    .field("key", &self.flags)
                    .finish()
            } else {
                f.debug_struct("Property::Ordinary")
                    .field("value", &self.value.value)
                    .field("key", &self.flags)
                    .finish()
            }
        }
    }
}

impl Property {
    pub fn accessor(accessor: Accessor, flags: PropertyFlags, key: Atom) -> Self {
        Property {
            flags: flags | PropertyFlags::ACCESSOR,
            key,
            value: PropertyUnion { accessor },
        }
    }

    pub fn value(value: Value, flags: PropertyFlags, key: Atom) -> Self {
        Property {
            flags,
            key,
            value: PropertyUnion { value },
        }
    }

    pub fn into_value(self) -> PropertyValue {
        unsafe {
            if self.flags.contains(PropertyFlags::ACCESSOR) {
                PropertyValue::Accessor(self.value.accessor)
            } else {
                PropertyValue::Value(self.value.value)
            }
        }
    }
}

unsafe impl Trace for Property {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        if self.flags.contains(PropertyFlags::ACCESSOR) {
            unsafe {
                self.value.accessor.get.trace(ctx);
                self.value.accessor.set.trace(ctx);
            }
        } else {
            unsafe {
                self.value.value.trace(ctx);
            }
        }
    }
}

#[derive(Debug)]
pub struct Properties {
    map: UnsafeCell<HashMap<Atom, usize>>,
    properties: CellVec<Property>,
}

unsafe impl Trace for Properties {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        unsafe { self.properties.unsafe_iter().for_each(|x| x.trace(ctx)) }
    }

    fn finalize(&self, atoms: &crate::atom::Atoms) {
        unsafe {
            self.properties.unsafe_iter().for_each(|x| {
                atoms.decrement(x.key);
            })
        }
    }
}

pub enum SetResult {
    Occupied,
    Vacant,
    Setter(Gc<Object>),
}

impl Properties {
    pub fn new() -> Self {
        Properties {
            map: UnsafeCell::new(HashMap::default()),
            properties: CellVec::new(),
        }
    }

    #[inline]
    pub fn lookup_idx(&self, atom: Atom) -> Option<usize> {
        unsafe { (*self.map.get()).get(&atom).copied() }
    }

    pub fn copy_properties(&self) -> Vec<Property> {
        self.properties.clone().into_inner()
    }

    pub fn get(&self, atom: Atom) -> Option<Property> {
        self.lookup_idx(atom)
            .and_then(|idx| self.properties.get(idx))
    }

    pub fn get_idx(&self, idx: usize) -> Option<Property> {
        self.properties.get(idx)
    }

    pub fn set_idx(&self, idx: usize, property: Property) {
        self.properties.set(idx, property)
    }

    pub fn set(&self, atom: Atom, value: Property) -> bool {
        unsafe {
            match (*self.map.get()).entry(atom) {
                Entry::Occupied(x) => {
                    self.properties.set(*x.get(), value);
                    false
                }
                Entry::Vacant(x) => {
                    x.insert(self.properties.len());
                    self.properties.push(value);
                    true
                }
            }
        }
    }

    pub fn set_value(&self, atom: Atom, value: Value) -> SetResult {
        unsafe {
            match (*self.map.get()).entry(atom) {
                Entry::Occupied(x) => {
                    let prop = self.properties.get(*x.get()).unwrap();
                    match prop.into_value() {
                        PropertyValue::Accessor(Accessor { set: Some(set), .. }) => {
                            return SetResult::Setter(set);
                        }
                        _ => {}
                    }
                    self.properties
                        .set(*x.get(), Property::value(value, prop.flags, prop.key));
                    SetResult::Occupied
                }
                Entry::Vacant(x) => {
                    x.insert(self.properties.len());
                    self.properties
                        .push(Property::value(value, PropertyFlags::ORDINARY, atom));
                    SetResult::Vacant
                }
            }
        }
    }
}
