//! A string interner

use crate::slotmap::{SlotKey, SlotVec};
use fxhash::FxHashMap;

newtype_key!(
    pub struct StringId(u32);
);

/// A string interner with the ability to remove interned strings by utilizing a manual reference
/// count.
pub struct Interner {
    map: FxHashMap<String, StringId>,
    values: SlotVec<(String, usize), StringId>,
}

impl Default for Interner {
    fn default() -> Self {
        Interner::new()
    }
}

impl Interner {
    /// Create a new interner

    pub fn new() -> Self {
        Interner {
            map: FxHashMap::default(),
            values: SlotVec::new(),
        }
    }

    /// Creates a new interner with a set of constant values
    /// It is assumed that all the constants are distinct.
    /// Constants cannot be freed.
    pub fn with_constants(constants: &[&str]) {
        let mut interner = Interner::new();
        for (idx, s) in constants.iter().enumerate() {
            let id = interner.values.insert(((*s).to_string(), usize::MAX));
            assert_eq!(id, StringId::new(idx));
            interner.map.insert((*s).to_string(), id);
        }
    }

    /// Intern a new string returning its id.
    /// If the string already existed its reference count gets incremented.
    /// Otherwise a new string is created with a reference count of 1.
    pub fn intern(&mut self, name: &str) -> StringId {
        if let Some(id) = self.map.get(name).copied() {
            if self.values[id].1 != usize::MAX {
                self.values[id].1 += 1;
            }
            return id;
        }
        let id = self.values.insert((name.to_string(), 1));
        self.map.insert(name.to_string(), id);
        id
    }

    /// Increment the reference count of an interned string with given id.
    pub fn increment(&mut self, id: StringId) {
        self.values[id].1 += 1;
    }

    /// Decrement the reference count of an intrened string freeing the string
    /// if it is no longer used.
    /// User needs to ensure that strings are freed properly as stringid can be reused so while
    /// freeing to many times wont lead to undefined behaviour. It might cause the freeing of strings
    /// which should not be freed.
    pub fn free(&mut self, id: StringId) {
        if self.values[id].1 != usize::MAX {
            self.values[id].1 -= 1;
        }
        if self.values[id].1 == 0 {
            self.values.remove(id);
        }
    }

    /// Returns the string associated with the id. Can be none if it was freed.
    pub fn lookup(&self, id: StringId) -> Option<&str> {
        self.values.get(id).map(|x| x.0.as_str())
    }
}
