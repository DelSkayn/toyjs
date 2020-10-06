//! A string interner

use fxhash::FxHashMap;

newtype_index!(
    pub struct StringId
);

pub enum InternValue {
    Present { string: String, count: usize },
    Free(Option<StringId>),
}

/// A string interner with the ability to remove interned strings by utilizing a manual reference
/// count.
pub struct Interner {
    map: FxHashMap<String, StringId>,
    values: Vec<InternValue>,
    top_free: Option<StringId>,
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
            values: Vec::new(),
            top_free: None,
        }
    }

    /// Creates a new interner with a set of constant values
    /// It is assumed that all the constants are distinct.
    /// Constants cannot be freed.
    pub fn with_constants(constants: &[&str]) {
        let mut interner = Interner::new();
        for s in constants {
            let idx = StringId::from(interner.values.len());
            interner.map.insert(s.to_string(), idx);
            interner.values.push(InternValue::Present {
                string: s.to_string(),
                count: usize::MAX,
            })
        }
    }

    /// Intern a new string returning its id.
    /// If the string already existed its reference count gets incremented.
    /// Otherwise a new string is created with a reference count of 1.
    pub fn intern(&mut self, name: &str) -> StringId {
        if let Some(&id) = self.map.get(name) {
            match self.values[usize::from(id)] {
                InternValue::Present {
                    string: _,
                    ref mut count,
                } => {
                    if *count != usize::MAX {
                        *count += 1;
                    }
                }
                _ => unreachable!(),
            }
            return id;
        }
        let id = self.allocate(name.to_string());
        self.map.insert(name.to_string(), id);
        id
    }

    fn allocate(&mut self, s: String) -> StringId {
        if let Some(x) = self.remove_free() {
            self.values[usize::from(x)] = InternValue::Present {
                string: s,
                count: 1,
            };
            return x;
        }
        let res = StringId::from(self.values.len());
        self.values.push(InternValue::Present {
            string: s,
            count: 1,
        });
        res
    }

    /// Increment the reference count of an interned string with given id.
    pub fn increment(&mut self, id: StringId) {
        match self.values[usize::from(id)] {
            InternValue::Present {
                string: _,
                ref mut count,
            } => {
                if *count != usize::MAX {
                    *count += 1;
                }
            }
            InternValue::Free(_) => {}
        }
    }

    /// Decrement the reference count of an intrened string freeing the string
    /// if it is no longer used.
    /// User needs to ensure that strings are freed properly as stringid can be reused so while
    /// freeing to many times wont lead to undefined behaviour. It might cause the freeing of strings
    /// which should not be freed.
    pub fn free(&mut self, id: StringId) {
        match self.values[usize::from(id)] {
            InternValue::Present {
                string: _,
                ref mut count,
            } => {
                if *count == usize::MAX {
                    return;
                }
                if *count > 1 {
                    *count -= 1;
                    return;
                }
            }
            _ => return,
        };
        self.insert_free(id);
    }

    fn insert_free(&mut self, idx: StringId) {
        self.map.remove(match self.values[usize::from(idx)] {
            InternValue::Present {
                ref string,
                count: _,
            } => string,
            _ => unreachable!(),
        });
        self.values[usize::from(idx)] = InternValue::Free(self.top_free);
        self.top_free = Some(idx);
    }

    fn remove_free(&mut self) -> Option<StringId> {
        let free = self.top_free?;
        let val = match self.values[usize::from(free)] {
            InternValue::Free(x) => x,
            _ => unreachable!(),
        };
        self.top_free = val;
        Some(free)
    }

    /// Returns the string associated with the id. Can be none if it was freed.
    pub fn lookup(&self, id: StringId) -> Option<&str> {
        match self.values.get(usize::from(id))? {
            InternValue::Present {
                ref string,
                count: _,
            } => Some(string.as_str()),
            _ => None,
        }
    }
}
