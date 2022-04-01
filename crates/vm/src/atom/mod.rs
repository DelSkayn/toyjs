use std::{
    cell::{Cell, UnsafeCell},
    fmt, mem,
};

use common::collections::HashMap;

use crate::{Realm, Value};

pub mod constant;

pub type AtomInt = u32;

pub const ATOM_MAX: AtomInt = (u32::MAX >> 1) as AtomInt;
pub const STRING_FLAG: AtomInt = !(u32::MAX >> 1) as AtomInt;

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub struct Atom(AtomInt);

impl Atom {
    pub const fn into_raw(self) -> AtomInt {
        self.0
    }

    pub const fn from_raw(v: AtomInt) -> Self {
        Atom(v)
    }

    const fn from_int(v: AtomInt) -> Self {
        Atom(v)
    }

    const fn from_string_idx(v: AtomInt) -> Self {
        Atom(v | STRING_FLAG)
    }

    pub const fn into_idx(self) -> Option<u32> {
        if self.0 & STRING_FLAG == 0 {
            Some(self.0)
        } else {
            None
        }
    }
}

#[derive(Debug)]
enum AtomEntry {
    Filled { text: Box<str>, count: usize },
    Empty { next: Option<AtomInt> },
}

pub struct Atoms {
    map: UnsafeCell<HashMap<Box<str>, AtomInt>>,
    entries: UnsafeCell<Vec<AtomEntry>>,
    empty: Cell<Option<AtomInt>>,
}

impl fmt::Debug for Atoms {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            f.debug_struct("Atoms")
                .field("map", &(*self.map.get()))
                .field("entries", &(*self.entries.get()))
                .field("empty", &self.empty)
                .finish()
        }
    }
}

impl Atoms {
    pub fn new() -> Self {
        let mut res = Atoms {
            entries: UnsafeCell::new(Vec::new()),
            map: UnsafeCell::new(HashMap::default()),
            empty: Cell::new(None),
        };
        constant::init_constants(&mut res);
        res
    }

    fn into_integer_string(s: &str) -> Option<u32> {
        let mut res = 0u32;
        for b in s.bytes() {
            if !(b'0'..=b'9').contains(&b) {
                return None;
            }
            let value = (b - b'0') as u32;
            res = res.checked_mul(10)?;
            res = res.checked_add(value)?;
        }
        if res < ATOM_MAX {
            Some(res)
        } else {
            None
        }
    }

    #[inline]
    pub unsafe fn atomize(&self, value: Value, realm: &Realm) -> Result<Atom, Value> {
        if let Some(a) = value.into_atom() {
            return Ok(a);
        }

        if let Some(i) = value.into_int().and_then(Self::atomize_int) {
            return Ok(i);
        }

        let str = realm.to_string(value)?;

        Ok(self.atomize_string(str.as_str()))
    }

    pub fn atomize_string(&self, text: &str) -> Atom {
        unsafe {
            if let Some(x) = Self::into_integer_string(text) {
                return Atom::from_int(x);
            }

            if let Some(x) = (*self.map.get()).get(text).copied() {
                if let AtomEntry::Filled { ref mut count, .. } = (*self.entries.get())[x as usize] {
                    *count += 1;
                } else {
                    unreachable!();
                }
                return Atom::from_string_idx(x);
            }

            let owned = text.to_string().into_boxed_str();

            if let Some(x) = self.empty.get() {
                let empty = mem::replace(
                    &mut (*self.entries.get())[x as usize],
                    AtomEntry::Filled {
                        text: owned.clone(),
                        count: 1,
                    },
                );
                if let AtomEntry::Empty { next } = empty {
                    self.empty.set(next);
                } else {
                    unreachable!();
                }
                (*self.map.get()).insert(owned, x);
                return Atom::from_string_idx(x);
            }

            let idx = (*self.entries.get())
                .len()
                .try_into()
                .expect("too many atoms");
            assert!(idx <= ATOM_MAX, "too many atoms");
            (*self.map.get()).insert(owned.clone(), idx);
            (*self.entries.get()).push(AtomEntry::Filled {
                text: owned,
                count: 1,
            });
            Atom::from_string_idx(idx)
        }
    }

    pub fn atomize_int(int: i32) -> Option<Atom> {
        if int > 0 && (int as u32) < ATOM_MAX {
            return Some(Atom::from_int(int as u32));
        }
        None
    }

    pub fn increment(&self, atom: Atom) {
        unsafe {
            if atom.0 & STRING_FLAG == 0 {
                return;
            }

            if let AtomEntry::Filled { ref mut count, .. } =
                (*self.entries.get())[(atom.0 & !STRING_FLAG) as usize]
            {
                *count = count.checked_add(1).unwrap();
            }
        }
    }

    pub fn decrement(&self, atom: Atom) {
        unsafe {
            if atom.0 & STRING_FLAG == 0 {
                return;
            }

            let idx = atom.0 & !STRING_FLAG;
            if let AtomEntry::Filled {
                ref mut count,
                ref text,
            } = (*self.entries.get())[idx as usize]
            {
                if *count == 1 {
                    (*self.map.get()).remove(text);
                    (*self.entries.get())[idx as usize] = AtomEntry::Empty {
                        next: self.empty.get(),
                    };
                    self.empty.set(Some(idx));
                } else {
                    *count -= 1;
                }
            }
        }
    }
}