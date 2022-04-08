use std::{
    cell::{RefCell, UnsafeCell},
    collections::BTreeMap,
    fmt,
};

use common::{cell_vec::CellVec, collections::HashMap};

use crate::{atom::Atom, gc::Trace, Gc, Realm, Value, VmInner};

mod function;
pub use function::{MutableFn, SharedFn, StaticFn, VmFunction, RECURSIVE_FUNC_PANIC};
mod index;

bitflags::bitflags! {
    pub struct ObjectFlags: u8{
        const EXTENABLE = 0b1;
        const CONSTRUCTOR = 0b10;
        const ORDINARY = Self::EXTENABLE.bits;
    }
}

impl ObjectFlags {}

bitflags::bitflags! {
    pub struct PropertyFlags: u8{
        const WRITABLE = 0b1;
        // Has either a getter and/or a setter
        const ENUMERABLE = 0b10;
        const CONFIGURABLE = 0b100;
        const ORDINARY = Self::WRITABLE.bits | Self::ENUMERABLE.bits | Self::CONFIGURABLE.bits;
        #[doc(hidden)]
        const __ACCESSOR = 0b1000;
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
            }
        )
    }
}

#[derive(Clone, Debug)]
pub struct Accessor {
    pub get: Option<Gc<Object>>,
    pub set: Option<Gc<Object>>,
}

impl Copy for Accessor {}

#[derive(Clone, Copy)]
union PropertyValue {
    accessor: Accessor,
    value: Value,
}

#[derive(Clone, Copy)]
pub(crate) struct Property {
    pub flags: PropertyFlags,
    value: PropertyValue,
    pub key: Atom,
}

impl fmt::Debug for Property {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            if self.flags.contains(PropertyFlags::__ACCESSOR) {
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
    #[inline]
    fn get_accessor(&self) -> Option<&Accessor> {
        if self.flags.contains(PropertyFlags::__ACCESSOR) {
            unsafe { Some(&self.value.accessor) }
        } else {
            None
        }
    }

    pub fn is_enumerable(&self) -> bool {
        self.flags.contains(PropertyFlags::ENUMERABLE)
    }

    pub unsafe fn get(&self, this: Gc<Object>, realm: &Realm) -> Result<Option<Value>, Value> {
        if self.flags.contains(PropertyFlags::__ACCESSOR) {
            if let Some(get) = self.value.accessor.get {
                if get.is_function() {
                    return realm.enter_method_call(get, this.into()).map(Some);
                } else {
                    return Err(realm.create_type_error("getter is not a function"));
                }
            } else {
                Ok(None)
            }
        } else {
            Ok(Some(self.value.value))
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
        unsafe {
            if let Some(accessor) = self.get_accessor() {
                if let Some(x) = accessor.get {
                    ctx.mark(x)
                };
                if let Some(x) = accessor.set {
                    ctx.mark(x)
                };
            } else {
                self.value.value.trace(ctx)
            }
        }
    }
}

enum ElementsInner {
    Array(Vec<Value>),
    Tree(BTreeMap<usize, Value>),
}

#[derive(Debug)]
pub struct Elements(UnsafeCell<ElementsInner>);

impl Elements {
    /// The ratio of empty values a entry must be from the previous in order to cause a fallback to a
    /// hashmap implementation
    const BACKDOWN_RATIO: f64 = 1.5;
    const BACKDOWN_MINIMUM: usize = 8;

    pub fn new() -> Self {
        Elements(UnsafeCell::new(ElementsInner::Array(Vec::new())))
    }

    pub fn set(&self, key: usize, v: Value) {
        unsafe {
            // Safe as we only hold the mutable reference within this scope and no references can
            // escape.
            match *self.0.get() {
                ElementsInner::Array(ref mut array) => {
                    if key >= array.len() {
                        if key > Self::BACKDOWN_MINIMUM
                            && key > (array.len() as f64 * Self::BACKDOWN_RATIO).floor() as usize
                        {
                            let mut map: BTreeMap<usize, Value> = array
                                .iter()
                                .copied()
                                .filter(|x| !x.is_empty())
                                .enumerate()
                                .collect();
                            map.insert(key, v);
                            (*self.0.get()) = ElementsInner::Tree(map);
                            return;
                        }
                        array.resize(key, Value::empty());
                        array.push(v);
                    } else {
                        // Safe because key bound is checked above
                        *array.get_unchecked_mut(key) = v
                    }
                }
                ElementsInner::Tree(ref mut tree) => {
                    tree.insert(key, v);
                }
            }
        }
    }

    pub fn get(&self, key: usize) -> Option<Value> {
        unsafe {
            // Safe as we only hold the mutable reference within this scope and no references can
            // escape.
            match *self.0.get() {
                ElementsInner::Array(ref array) => {
                    array
                        .get(key)
                        .copied()
                        .and_then(|x| if x.is_empty() { None } else { Some(x) })
                }
                ElementsInner::Tree(ref tree) => tree.get(&key).copied(),
            }
        }
    }

    pub fn for_each<F: FnMut(usize, Value)>(&self, mut f: F) {
        unsafe {
            match *self.0.get() {
                ElementsInner::Array(ref array) => array
                    .iter()
                    .copied()
                    .enumerate()
                    .filter(|x| !x.1.is_empty())
                    .for_each(|(a, b)| f(a, b)),
                ElementsInner::Tree(ref t) => {
                    t.iter().map(|(a, b)| (*a, *b)).for_each(|(a, b)| f(a, b))
                }
            }
        }
    }
}

unsafe impl Trace for Elements {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        unsafe {
            // Safe as we only hold the mutable reference within this scope and no references can
            // escape.
            match *self.0.get() {
                ElementsInner::Array(ref x) => {
                    x.iter().for_each(|x| x.trace(ctx));
                }
                ElementsInner::Tree(ref x) => {
                    x.values().for_each(|x| x.trace(ctx));
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct Object {
    pub prototype: Option<Gc<Object>>,
    pub flags: ObjectFlags,

    pub(crate) map: UnsafeCell<HashMap<Atom, usize>>,
    pub(crate) properties: CellVec<Property>,

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
        if let Some(x) = self.prototype {
            ctx.mark(x)
        };
        self.elements.trace(ctx);
        self.kind.trace(ctx);
        unsafe {
            // Safe as none of the references from the iterator escape the scope.
            self.properties.unsafe_iter().for_each(|x| x.trace(ctx));
        }
    }

    fn finalize(&self, atoms: &crate::atom::Atoms) {
        unsafe {
            (*self.map.get())
                .keys()
                .copied()
                .for_each(|x| atoms.decrement(x))
        }
    }
}

impl Object {
    pub fn new(prototype: Option<Gc<Object>>, flags: ObjectFlags, kind: ObjectKind) -> Self {
        Object {
            prototype,
            flags,
            kind,

            map: UnsafeCell::new(HashMap::default()),
            properties: CellVec::new(),

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
