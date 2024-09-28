use std::{
    hash::{Hash, Hasher},
    hint::unreachable_unchecked,
    ops::Index,
};

use common::string::String;
use hashbrown::{raw::RawTable, Equivalent};

#[derive(Clone, Copy, Hash, Eq, PartialEq)]
pub struct Atom(u32);

pub enum AtomMapSlot {
    Free { next: Option<u32> },
    Used { hash: u64, string: String },
}

pub struct AtomMap {
    map: RawTable<Atom>,
    storage: Vec<AtomMapSlot>,
    free: Option<u32>,
}

impl AtomMap {
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
                if self.storage.len() == u32::MAX as usize {
                    return None;
                }

                let atom = Atom(self.storage.len() as u32);
                unsafe {
                    self.map.insert_in_slot(hash, slot, atom);
                }

                self.storage.push(AtomMapSlot::Used {
                    hash,
                    string: String::from(key),
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
