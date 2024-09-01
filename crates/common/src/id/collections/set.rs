use std::hash::{BuildHasher, Hash, Hasher, RandomState};

use hashbrown::raw::RawTable;

use crate::id::{collections::IdVec, Id, IdRangeError};

/// A collection which will ensure that the storage only contains unique values.
/// If two values are pushed which are equal to each-other this collection will instead return the
/// id of the previous value.
#[derive(Default)]
pub struct IdSet<I, V, S = RandomState> {
    map: RawTable<I>,
    storage: IdVec<I, V>,
    hasher: S,
}

impl<I, V> IdSet<I, V>
where
    I: Id,
    V: Eq + Hash,
{
    pub fn new() -> Self {
        IdSet {
            map: RawTable::new(),
            storage: IdVec::new(),
            hasher: RandomState::new(),
        }
    }
}

impl<I, V, S> IdSet<I, V, S>
where
    I: Id,
    V: Eq + Hash,
    S: BuildHasher,
{
    pub fn push(&mut self, v: V) -> Result<I, IdRangeError> {
        let mut hasher = self.hasher.build_hasher();
        v.hash(&mut hasher);
        let hash = hasher.finish();
        match self.map.find_or_find_insert_slot(
            hash,
            |s| self.storage[*s] == v,
            |s| {
                let mut hasher = self.hasher.build_hasher();
                self.storage[*s].hash(&mut hasher);
                hasher.finish()
            },
        ) {
            Ok(x) => unsafe { Some(*x.as_ref()) },
            Err(slot) => {
                let index = self.storage.push(v)?;
                unsafe { self.map.insert_in_slot(hash, slot, index) };
                Ok(index)
            }
        }
    }

    pub fn get(&self, index: I) -> Option<&V> {
        self.storage.get(index)
    }

    pub fn get_mut(&mut self, index: I) -> Option<&mut V> {
        self.storage.get_mut(index)
    }

    pub fn clear(&mut self) {
        self.map.clear();
        self.storage.clear();
    }

    pub fn len(&self) -> usize {
        self.storage.len()
    }

    pub fn is_empty(&self) -> bool {
        self.storage.is_empty()
    }
}
