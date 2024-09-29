use std::{
    hash::{Hash, Hasher},
    hint::unreachable_unchecked,
    ops::Index,
};

use common::string::String;
use hashbrown::{raw::RawTable, Equivalent};

#[derive(Clone, Copy, Hash, Eq, PartialEq)]
pub struct Atom(u32);

impl Atom {
    pub const INVALID: Atom = Atom(u32::MAX);
}

pub enum Mark {
    Marked,
    Unmarked,
}

pub enum AtomMapSlot {
    Free {
        next: Option<u32>,
    },
    Used {
        hash: u64,
        string: String,
        mark: Mark,
    },
}

pub struct AtomMap {
    map: RawTable<Atom>,
    storage: Vec<AtomMapSlot>,
    free: Option<u32>,
}

impl AtomMap {
    pub const MAX_ATOM: Atom = Atom(u32::MAX - 1);

    pub fn new() -> Self {
        AtomMap {
            map: RawTable::new(),
            storage: Vec::new(),
            free: None,
        }
    }

    pub fn sweep(&mut self) {
        for (idx, i) in self.storage.iter_mut().enumerate() {
            let idx = idx as u32;
            let AtomMapSlot::Used { hash, mark, .. } = i else {
                continue;
            };
            if let Mark::Marked = std::mem::replace(mark, Mark::Unmarked) {
                continue;
            }
            self.map.remove_entry(*hash, |x| x.0 == idx).unwrap();
            let free = self.free.replace(idx);
            *i = AtomMapSlot::Free { next: free };
        }
    }

    pub fn mark(&mut self, atom: Atom) {
        let AtomMapSlot::Used { ref mut mark, .. } = self.storage[atom.0 as usize] else {
            panic!("tried to mark atom which was already free");
        };
        *mark = Mark::Marked;
    }

    /// Get the atom value for the given string.
    ///
    /// Returns None if the atom map storage is full.
    pub fn get_or_insert<K>(&mut self, key: &K) -> Option<Atom>
    where
        String: Equivalent<K> + for<'a> From<&'a K>,
        K: Hash,
    {
        let mut hasher = ahash::AHasher::default();
        key.hash(&mut hasher);
        let hash = hasher.finish();

        let lookup = self.map.find_or_find_insert_slot(
            hash,
            |t| unsafe {
                let AtomMapSlot::Used { ref string, .. } = self.storage.get_unchecked(t.0 as usize)
                else {
                    if cfg!(debug_assertions) {
                        unreachable!()
                    } else {
                        unreachable_unchecked()
                    }
                };
                string.equivalent(key)
            },
            |h| unsafe {
                let AtomMapSlot::Used { hash, .. } = self.storage.get_unchecked(h.0 as usize)
                else {
                    if cfg!(debug_assertions) {
                        unreachable!()
                    } else {
                        unreachable_unchecked()
                    }
                };
                *hash
            },
        );

        match lookup {
            Ok(bucket) => Some(unsafe { *bucket.as_ref() }),
            Err(slot) => {
                if self.storage.len() == Self::MAX_ATOM.0 as usize {
                    return None;
                }

                let atom = Atom(self.storage.len() as u32);
                unsafe {
                    self.map.insert_in_slot(hash, slot, atom);
                }

                self.storage.push(AtomMapSlot::Used {
                    hash,
                    string: String::from(key),
                    mark: Mark::Unmarked,
                });

                Some(atom)
            }
        }
    }

    pub fn get<K>(&self, k: &K) -> Option<Atom>
    where
        String: Equivalent<K>,
        K: Hash,
    {
        let mut hasher = ahash::AHasher::default();
        k.hash(&mut hasher);
        let hash = hasher.finish();

        self.map
            .get(hash, |t| unsafe {
                let AtomMapSlot::Used { ref string, .. } = self.storage.get_unchecked(t.0 as usize)
                else {
                    if cfg!(debug_assertions) {
                        unreachable!()
                    } else {
                        unreachable_unchecked()
                    }
                };
                string.equivalent(k)
            })
            .copied()
    }
}

impl Index<Atom> for AtomMap {
    type Output = String;

    fn index(&self, index: Atom) -> &Self::Output {
        match self.storage[index.0 as usize] {
            AtomMapSlot::Free { .. } => {
                panic!("Used atom which was freed!")
            }
            AtomMapSlot::Used { ref string, .. } => string,
        }
    }
}
