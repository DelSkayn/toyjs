use core::fmt;
use std::{collections::HashMap, hash::Hash, marker::PhantomData};

use dreck::{rebind, Bound, Gc, Root, Trace, WeakGc};

pub mod constants;

pub trait IntoAtom<'own> {
    fn into_atom<'gc>(self, root: &'gc Root<'own>, atoms: &mut Atoms<'gc, 'own>)
        -> Atom<'gc, 'own>;
}

impl<'own> IntoAtom<'own> for &str {
    fn into_atom<'gc>(
        self,
        root: &'gc Root<'own>,
        atoms: &mut Atoms<'gc, 'own>,
    ) -> Atom<'gc, 'own> {
        atoms.atomize_string(root, self)
    }
}

impl<'own> IntoAtom<'own> for Atom<'_, 'own> {
    fn into_atom<'gc>(
        self,
        root: &'gc Root<'own>,
        _atoms: &mut Atoms<'gc, 'own>,
    ) -> Atom<'gc, 'own> {
        rebind!(root, self)
    }
}

impl<'a, 'own, F> IntoAtom<'own> for F
where
    F: FnOnce() -> Atom<'a, 'own>,
{
    fn into_atom<'gc>(
        self,
        root: &'gc Root<'own>,
        _atoms: &mut Atoms<'gc, 'own>,
    ) -> Atom<'gc, 'own> {
        rebind!(root, self())
    }
}

pub struct AtomData(String);

unsafe impl<'own> Trace<'own> for AtomData {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        false
    }

    fn trace<'a>(&self, _: dreck::Tracer<'a, 'own>) {}
}

unsafe impl<'to> Bound<'to> for AtomData {
    type Rebound = Self;
}

#[derive(Clone, Copy)]
pub struct Atom<'gc, 'own> {
    ptr: *const AtomData,
    marker: PhantomData<Gc<'gc, 'own, AtomData>>,
}

impl fmt::Debug for Atom<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(x) = self.into_integer() {
            f.debug_tuple("Atom::integer").field(&x).finish()
        } else if let Some(x) = self.into_constant_id() {
            f.debug_tuple("Atom::constant")
                .field(&constants::STRINGS[x])
                .finish()
        } else {
            assert!(self.is_pointer());
            unsafe { f.debug_tuple("Atom::string").field(&(*self.ptr).0).finish() }
        }
    }
}

impl<'gc, 'own> Hash for Atom<'gc, 'own> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.ptr as usize).hash(state)
    }
}

impl<'gc, 'own> PartialEq for Atom<'gc, 'own> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr as usize == other.ptr as usize
    }
}
impl<'gc, 'own> Eq for Atom<'gc, 'own> {}

impl<'gc, 'own> PartialOrd for Atom<'gc, 'own> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (self.ptr as usize).partial_cmp(&(other.ptr as usize))
    }
}

impl<'gc, 'own> Ord for Atom<'gc, 'own> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.ptr as usize).cmp(&(other.ptr as usize))
    }
}

unsafe impl<'from, 'to, 'own> Bound<'to> for Atom<'from, 'own> {
    type Rebound = Atom<'to, 'own>;
}

unsafe impl<'gc, 'own> Trace<'own> for Atom<'gc, 'own> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, tracer: dreck::Tracer<'a, 'own>) {
        self.into_gc().trace(tracer);
    }
}

impl<'gc, 'own> Atom<'gc, 'own> {
    const TAG_MASK: usize = 0b01;

    const TAG_PTR: usize = 0b00;
    const TAG_INTEGER: usize = 0b01;
    const TAG_CONSTANT: usize = 0b10;

    const MAX_INTEGER: usize = (u32::MAX >> 2) as usize;

    pub fn is_integer(self) -> bool {
        (self.ptr as usize) & Self::TAG_MASK == Self::TAG_INTEGER
    }

    pub fn into_integer(self) -> Option<usize> {
        if self.is_integer() {
            Some((self.ptr as usize) >> 2)
        } else {
            None
        }
    }

    pub fn is_constant(self) -> bool {
        (self.ptr as usize) & Self::TAG_MASK == Self::TAG_CONSTANT
    }

    pub fn into_constant_id(self) -> Option<usize> {
        if self.is_constant() {
            Some((self.ptr as usize) >> 2)
        } else {
            None
        }
    }

    pub fn is_pointer(self) -> bool {
        (self.ptr as usize) & Self::TAG_MASK == Self::TAG_PTR
    }

    pub fn into_gc(self) -> Option<Gc<'gc, 'own, AtomData>> {
        if self.is_pointer() {
            unsafe { Some(Gc::from_raw(self.ptr)) }
        } else {
            None
        }
    }

    pub fn from_gc(ptr: Gc<'gc, 'own, AtomData>) -> Self {
        Self {
            ptr: Gc::as_raw(ptr),
            marker: PhantomData,
        }
    }

    pub fn into_raw(self) -> *const AtomData {
        self.ptr
    }

    pub unsafe fn from_raw(ptr: *const AtomData) -> Self {
        Self {
            ptr,
            marker: PhantomData,
        }
    }
}

impl<'own> Atom<'static, 'own> {
    pub unsafe fn from_integer(value: usize) -> Self {
        debug_assert!(value < Self::MAX_INTEGER);
        Self {
            ptr: (value << 2 | Self::TAG_PTR) as *mut AtomData,
            marker: PhantomData,
        }
    }

    const fn from_constant_id(id: usize) -> Self {
        Self {
            ptr: (id << 2 | Self::TAG_CONSTANT) as *mut AtomData,
            marker: PhantomData,
        }
    }
}

pub enum AtomKind<'gc, 'own> {
    Allocated(WeakGc<'gc, 'own, AtomData>),
    Constant(Atom<'static, 'own>),
}

impl fmt::Debug for AtomKind<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            AtomKind::Allocated(x) => {
                if let Ok(x) = WeakGc::upgrade(x) {
                    f.debug_tuple("AtomKind::Allocated")
                        .field(unsafe { &(*Gc::as_raw(x)).0 })
                        .finish()
                } else {
                    f.debug_tuple("AtomKind::Deallocated").finish()
                }
            }
            AtomKind::Constant(x) => f.debug_tuple("AtomKind::Constant").field(&x).finish(),
        }
    }
}

#[derive(Debug)]
pub struct Atoms<'gc, 'own>(HashMap<String, AtomKind<'gc, 'own>>);

impl<'gc, 'own> Atoms<'gc, 'own> {
    pub fn new() -> Self {
        let mut hash_map = HashMap::new();
        for (idx, s) in constants::STRINGS.iter().enumerate() {
            hash_map.insert(
                s.to_string(),
                AtomKind::Constant(Atom::from_constant_id(idx)),
            );
        }
        Atoms(hash_map)
    }

    pub fn clean(&mut self) {
        self.0.retain(|_, v| {
            if let AtomKind::Allocated(ref x) = *v {
                !x.is_removed()
            } else {
                true
            }
        });
    }

    fn is_integer_string(s: &str) -> Option<usize> {
        let mut accum = 0usize;
        for c in s.chars() {
            let c = c.to_digit(10)?;
            accum = accum.checked_mul(10)?;
            accum = accum.checked_add(c as usize)?;
        }
        if accum <= Atom::MAX_INTEGER {
            Some(accum)
        } else {
            None
        }
    }

    fn atomize_string_ensured<'l>(&mut self, root: &'l Root<'own>, s: String) -> Atom<'l, 'own> {
        let value = self.0.entry(s.to_string()).or_insert_with(|| {
            let root = root.add(AtomData(s.to_string()));
            unsafe { AtomKind::Allocated(dreck::rebind(WeakGc::new(root))) }
        });

        match *value {
            AtomKind::Allocated(ref x) => {
                if let Ok(x) = WeakGc::upgrade(*x) {
                    return rebind!(root, Atom::from_gc(x));
                }
            }
            AtomKind::Constant(x) => return x,
        }

        let root = root.add(AtomData(s.to_string()));
        *value = AtomKind::Allocated(unsafe { dreck::rebind(WeakGc::new(root)) });

        Atom::from_gc(root)
    }

    pub fn atomize_integer<'l>(&mut self, root: &'l Root<'own>, integer: i32) -> Atom<'l, 'own> {
        if integer > 0 && integer as usize <= Atom::MAX_INTEGER {
            return unsafe { Atom::from_integer(integer as usize) };
        }

        self.atomize_string_ensured(root, format!("{integer}"))
    }

    pub fn atomize_string<'l>(&mut self, root: &'l Root<'own>, s: &str) -> Atom<'l, 'own> {
        if let Some(x) = Self::is_integer_string(s) {
            return unsafe { Atom::from_integer(x) };
        }
        self.atomize_string_ensured(root, s.to_string())
    }
}

unsafe impl<'own> Trace<'own> for Atoms<'_, 'own> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, tracer: dreck::Tracer<'a, 'own>) {
        for f in self.0.values() {
            match *f {
                AtomKind::Constant(_) => {}
                AtomKind::Allocated(x) => {
                    x.trace(tracer);
                }
            }
        }
    }
}
