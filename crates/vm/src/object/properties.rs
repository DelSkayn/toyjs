use core::fmt;
use std::{
    collections::hash_map::{Entry, VacantEntry},
    mem,
    ops::{BitOr, BitOrAssign},
    slice::Iter,
};

use common::{
    atom::{self, Atom, Atoms},
    collections::HashMap,
};

use crate::{
    cell::CellOwner,
    gc::{self, Arena, Rebind, Trace, Tracer},
    realm::GcRealm,
    rebind, rebind_try, root_clone, Value,
};

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

#[derive(Clone, Copy, Debug)]
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
    flags: PropertyFlags,
    atom: Atom,
    value: PropertyUnion<'gc, 'cell>,
}

impl<'gc, 'cell> fmt::Debug for Property<'gc, 'cell> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            if self.flags.contains(PropertyFlags::ACCESSOR) {
                f.debug_struct("Property::Accessor")
                    .field("value", &self.value.accessor)
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

unsafe impl<'a, 'gc, 'cell> Rebind<'a> for Property<'gc, 'cell> {
    type Output = Property<'a, 'cell>;
}

impl<'gc, 'cell> Property<'gc, 'cell> {
    pub fn accessor(accessor: Accessor<'gc, 'cell>, flags: PropertyFlags, atom: Atom) -> Self {
        Property {
            flags: flags | PropertyFlags::ACCESSOR,
            atom,
            value: PropertyUnion { accessor },
        }
    }

    pub fn value(value: Value<'gc, 'cell>, flags: PropertyFlags, atom: Atom) -> Self {
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

    pub fn as_value(&self) -> PropertyValue<'gc, 'cell> {
        unsafe {
            if self.flags.contains(PropertyFlags::ACCESSOR) {
                PropertyValue::Accessor(self.value.accessor)
            } else {
                PropertyValue::Value(self.value.value)
            }
        }
    }

    pub fn atom(&self) -> Atom {
        self.atom
    }

    pub fn from_value(
        owner: &mut CellOwner<'cell>,
        arena: &'gc mut Arena<'_, 'cell>,
        atoms: &Atoms,
        realm: GcRealm<'_, 'cell>,
        value: Value<'_, 'cell>,
        atom: Atom,
    ) -> Result<Self, Value<'gc, 'cell>> {
        let obj = value.into_object().ok_or_else(|| {
            realm.create_type_error(
                owner,
                arena,
                atoms,
                "Cannot build property from non-object value",
            )
        });
        let obj = rebind_try!(arena, obj);
        Self::from_object(owner, arena, atoms, realm, obj, atom)
    }

    pub fn from_object(
        owner: &mut CellOwner<'cell>,
        arena: &'gc mut Arena<'_, 'cell>,
        atoms: &Atoms,
        realm: GcRealm<'_, 'cell>,
        obj: GcObject<'_, 'cell>,
        atom: Atom,
    ) -> Result<Self, Value<'gc, 'cell>> {
        let mut flags = PropertyFlags::empty();
        let f = rebind_try!(
            arena,
            obj.index(owner, arena, atoms, realm, atom::constant::enumerable)
        );
        if !realm.is_falsish(owner, f) {
            flags |= PropertyFlags::ENUMERABLE
        }
        let f = rebind_try!(
            arena,
            obj.index(owner, arena, atoms, realm, atom::constant::configurable)
        );
        if !realm.is_falsish(owner, f) {
            flags |= PropertyFlags::CONFIGURABLE
        }

        let value = rebind_try!(
            arena,
            obj.index(owner, arena, atoms, realm, atom::constant::value)
        );

        let value = if value.is_empty() { None } else { Some(value) };
        root_clone!(arena, value);

        let f = rebind_try!(
            arena,
            obj.index(owner, arena, atoms, realm, atom::constant::writable)
        );
        if !realm.is_falsish(owner, f) {
            flags |= PropertyFlags::WRITABLE
        }

        let get = rebind_try!(
            arena,
            obj.index(owner, arena, atoms, realm, atom::constant::get)
        );
        root_clone!(arena, get);
        let get = if get.is_empty() {
            None
        } else {
            let get = get
                .into_object()
                .and_then(|x| {
                    if x.borrow(owner).is_function() {
                        Some(x)
                    } else {
                        None
                    }
                })
                .ok_or_else(|| {
                    realm.create_type_error(owner, arena, atoms, "getter is not a function")
                });

            Some(rebind_try!(arena, get))
        };

        let set = rebind_try!(
            arena,
            obj.index(owner, arena, atoms, realm, atom::constant::set)
        );
        let set = rebind!(arena, set);
        let set = if set.is_empty() {
            None
        } else {
            let set = set
                .into_object()
                .and_then(|x| {
                    if x.borrow(owner).is_function() {
                        Some(x)
                    } else {
                        None
                    }
                })
                .ok_or_else(|| {
                    realm.create_type_error(owner, arena, atoms, "setter is not a function")
                });
            Some(rebind_try!(arena, set))
        };

        if get.is_some() || set.is_some() {
            if value.is_some() {
                return Err(realm.create_type_error(
                    owner,
                    arena,
                    atoms,
                    "Object property cannot have both a value and an accessor",
                ));
            }
            if flags.contains(PropertyFlags::WRITABLE) {
                return Err(realm.create_type_error(
                    owner,
                    arena,
                    atoms,
                    "Object property cannot have both be writeable and have an accessor",
                ));
            }
            let get = rebind!(arena, get);
            let set = rebind!(arena, set);
            Ok(Property::accessor(Accessor { get, set }, flags, atom))
        } else {
            let value = rebind!(arena, value);
            Ok(Property::value(
                value.unwrap_or_else(Value::undefined),
                flags,
                atom,
            ))
        }
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
            if self.flags.contains(PropertyFlags::ACCESSOR) {
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

#[derive(Debug)]
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
            .push(Property::value(value, PropertyFlags::ordinary(), key));
    }
}

#[derive(Debug)]
pub struct Occupied<'a, 'gc, 'cell>(&'a mut Value<'gc, 'cell>);

impl<'a, 'gc, 'cell> Occupied<'a, 'gc, 'cell> {
    pub fn set(&mut self, value: Value<'_, 'cell>) {
        // Safe as the value from the reference will be kept alive for 'gc
        *self.0 = unsafe { gc::rebind(value) };
    }
}

#[derive(Debug)]
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

    pub fn entry<'a>(&'a mut self, atom: Atom) -> PropertyEntry<'a, 'gc, 'cell> {
        match self.map.entry(atom) {
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

    pub fn iter(&self) -> Iter<Property<'gc, 'cell>> {
        self.props.iter()
    }
}
