use std::{
    cell::UnsafeCell,
    collections::hash_map::Entry,
    fmt,
    ops::{BitOr, BitOrAssign},
};

use common::{cell_vec::CellVec, collections::HashMap};

use crate::{
    atom::{self, Atom},
    gc::Trace,
    Gc, Object, Realm, Value,
};

#[derive(Clone, Copy)]
pub struct PropertyFlags(u8);

impl PropertyFlags {
    pub const WRITABLE: PropertyFlags = PropertyFlags(0b1);
    pub const ENUMERABLE: PropertyFlags = PropertyFlags(0b10);
    pub const CONFIGURABLE: PropertyFlags = PropertyFlags(0b100);

    pub const ORDINARY: PropertyFlags =
        PropertyFlags(Self::WRITABLE.0 | Self::ENUMERABLE.0 | Self::CONFIGURABLE.0);

    pub const BUILTIN: PropertyFlags = PropertyFlags(Self::WRITABLE.0 | Self::CONFIGURABLE.0);

    const ACCESSOR: PropertyFlags = PropertyFlags(0b1000);

    #[inline]
    pub fn empty() -> Self {
        PropertyFlags(0)
    }

    #[inline]
    pub fn contains(self, flag: PropertyFlags) -> bool {
        (self.0 & flag.0) == flag.0
    }

    #[inline]
    #[must_use]
    pub fn clear(self, flag: PropertyFlags) -> Self {
        PropertyFlags(self.0 & !flag.0)
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

impl BitOrAssign for PropertyFlags {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Accessor {
    pub get: Option<Gc<Object>>,
    pub set: Option<Gc<Object>>,
}

pub enum PropertyValue {
    Accessor(Accessor),
    Value(Value),
}

impl PropertyValue {
    pub unsafe fn get(self, realm: &Realm, this: Gc<Object>) -> Result<Option<Value>, Value> {
        match self {
            PropertyValue::Value(x) => Ok(Some(x)),
            PropertyValue::Accessor(Accessor { get: Some(get), .. }) => {
                realm.enter_method_call(get, this.into()).map(Some)
            }
            _ => Ok(None),
        }
    }
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
                    .field("key", &self.key)
                    .finish()
            } else {
                f.debug_struct("Property::Ordinary")
                    .field("value", &self.value.value)
                    .field("flags", &self.flags)
                    .field("key", &self.key)
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

    pub fn is_accessor(&self) -> bool {
        self.flags.contains(PropertyFlags::ACCESSOR)
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

    pub unsafe fn from_value(realm: &Realm, value: Value, atom: Atom) -> Result<Self, Value> {
        let obj = value.into_object().ok_or_else(|| {
            realm.create_type_error("can't build properties from non object value")
        })?;
        Self::from_object(realm, obj, atom)
    }

    pub unsafe fn from_object(realm: &Realm, obj: Gc<Object>, atom: Atom) -> Result<Self, Value> {
        let mut flags = PropertyFlags::empty();
        if let Some(x) = obj.properties.get(atom::constant::enumerable) {
            if let Some(x) = x.into_value().get(realm, obj)? {
                if realm.is_falsish(x) {
                    flags |= PropertyFlags::ENUMERABLE;
                }
            }
        };
        if let Some(x) = obj.properties.get(atom::constant::configurable) {
            if let Some(x) = x.into_value().get(realm, obj)? {
                if realm.is_falsish(x) {
                    flags |= PropertyFlags::CONFIGURABLE;
                }
            }
        };

        let value = if let Some(x) = obj.properties.get(atom::constant::writable) {
            x.into_value().get(realm, obj)?
        } else {
            None
        };

        if let Some(x) = obj.properties.get(atom::constant::writable) {
            if let Some(x) = x.into_value().get(realm, obj)? {
                if realm.is_falsish(x) {
                    flags |= PropertyFlags::WRITABLE;
                }
            }
        };

        let get = if let Some(x) = obj.properties.get(atom::constant::get) {
            if let Some(get) = x.into_value().get(realm, obj)? {
                if let Some(x) = get.into_object() {
                    if !x.is_function() {
                        return Err(realm.create_type_error("getter is not a function"));
                    }
                    Some(x)
                } else {
                    return Err(realm.create_type_error("getter is not a function"));
                }
            } else {
                None
            }
        } else {
            None
        };
        let set = if let Some(x) = obj.properties.get(atom::constant::set) {
            if let Some(set) = x.into_value().get(realm, obj)? {
                if let Some(x) = set.into_object() {
                    if !x.is_function() {
                        return Err(realm.create_type_error("setter is not a function"));
                    }
                    Some(x)
                } else {
                    return Err(realm.create_type_error("setter is not a function"));
                }
            } else {
                None
            }
        } else {
            None
        };
        if get.is_some() || set.is_some() {
            if value.is_some() {
                return Err(realm
                    .create_type_error("object property can't have both a value and an accessor"));
            }
            if flags.contains(PropertyFlags::WRITABLE) {
                return Err(realm.create_type_error(
                    "object property can't both be writable and have an accessor",
                ));
            }
            Ok(Property::accessor(Accessor { get, set }, flags, atom))
        } else {
            Ok(Property::value(
                value.unwrap_or(Value::undefined()),
                flags,
                atom,
            ))
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

pub struct Properties {
    map: UnsafeCell<HashMap<Atom, usize>>,
    properties: CellVec<Property>,
}

impl fmt::Debug for Properties {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Properties")
            .field("map", unsafe { &(*self.map.get()) })
            .field("properties", &self.properties)
            .finish()
    }
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

pub enum GetResult {
    None,
    Value(Value),
    Getter(Gc<Object>),
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

    pub fn clone_properties(&self) -> Vec<Property> {
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

    pub fn freeze(&self) {
        unsafe {
            let len = self.properties.len();
            let ptr = self.properties.as_mut_ptr();
            for i in 0..len {
                let ptr = ptr.add(i);
                (*ptr).flags = (*ptr)
                    .flags
                    .clear(PropertyFlags::WRITABLE | PropertyFlags::CONFIGURABLE);
            }
        }
    }

    pub fn seal(&self) {
        unsafe {
            let len = self.properties.len();
            let ptr = self.properties.as_mut_ptr();
            for i in 0..len {
                let ptr = ptr.add(i);
                (*ptr).flags = (*ptr).flags.clear(PropertyFlags::CONFIGURABLE);
            }
        }
    }

    pub fn set(&self, value: Property) -> bool {
        unsafe {
            match (*self.map.get()).entry(value.key) {
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

    pub fn get_value(&self, atom: Atom) -> GetResult {
        unsafe {
            if let Some(idx) = (*self.map.get()).get(&atom).copied() {
                match self.properties.get(idx).unwrap().into_value() {
                    PropertyValue::Value(x) => GetResult::Value(x),
                    PropertyValue::Accessor(Accessor { get: Some(get), .. }) => {
                        GetResult::Getter(get)
                    }
                    _ => GetResult::None,
                }
            } else {
                GetResult::None
            }
        }
    }

    pub fn len(&self) -> usize {
        self.properties.len()
    }
}
