//! A string interner
//!
//! based on the blog post: https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html

use crate::util::{Index, OptionIndex};
use fxhash::FxHashMap;
use std::mem;

/// Some preloaded strings in the interner often used
pub mod consts {
    use super::StringId;
    use crate::util::Index;

    pub(crate) const PRELOAD: [&str; 8] = [
        "target",
        "get",
        "set",
        "static",
        "of",
        "import",
        "meta",
        "use strict",
    ];

    pub const TARGET: StringId = StringId(Index(0));
    pub const GET: StringId = StringId(Index(1));
    pub const SET: StringId = StringId(Index(2));
    pub const STATIC: StringId = StringId(Index(3));
    pub const OF: StringId = StringId(Index(4));
    pub const IMPORT: StringId = StringId(Index(5));
    pub const META: StringId = StringId(Index(6));
    pub const STRICT_DIRECTIVE: StringId = StringId(Index(7));
}

shrinkwrap_index!(StringId, OptionStringId);

pub enum InternValue {
    Present { string: String, count: usize },
    Free(OptionIndex),
}

pub struct Interner {
    map: FxHashMap<String, Index>,
    values: Vec<InternValue>,
    top_free: OptionIndex,
}

impl Default for Interner {
    fn default() -> Self {
        Interner::new()
    }
}

impl Interner {
    pub fn new() -> Self {
        let mut res = Interner {
            map: FxHashMap::default(),
            values: Vec::new(),
            top_free: OptionIndex::none(),
        };
        for (i, p) in consts::PRELOAD.iter().enumerate() {
            let id = res.intern(p);
            debug_assert_eq!(usize::from(id.0), i);
        }
        res
    }

    fn preallocate(&mut self, name: &str) -> StringId {
        let idx = Index::from(self.values.len());
        self.values.push(InternValue::Present {
            string: name.to_string(),
            count: usize::MAX,
        });
        self.map.insert(name.to_string(), idx);
        StringId(Index::from(idx))
    }

    pub fn intern(&mut self, name: &str) -> StringId {
        if let Some(&id) = self.map.get(name) {
            return StringId(id);
        }
        let id = self.allocate(name.to_string());
        self.map.insert(name.to_string(), id);
        StringId(id)
    }

    fn allocate(&mut self, s: String) -> Index {
        if let Some(x) = self.remove_free() {
            self.values[x.into_usize()] = InternValue::Present {
                string: s,
                count: 1,
            };
            return x;
        }
        let res = Index::from(self.values.len());
        self.values.push(InternValue::Present {
            string: s,
            count: 1,
        });
        res
    }

    pub fn increment(&mut self, id: StringId) {
        match self.values[id.0.into_usize()] {
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

    pub fn free(&mut self, id: StringId) {
        match self.values[id.0.into_usize()] {
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
        self.insert_free(id.0);
    }

    fn insert_free(&mut self, idx: Index) {
        self.map.remove(match self.values[idx.into_usize()] {
            InternValue::Present {
                ref string,
                count: _,
            } => string,
            _ => unreachable!(),
        });
        self.values[idx.into_usize()] = InternValue::Free(self.top_free);
        self.top_free = OptionIndex::some(idx);
    }

    fn remove_free(&mut self) -> Option<Index> {
        let free = self.top_free.into_option()?;
        let val = match self.values[free.into_usize()] {
            InternValue::Free(x) => x,
            _ => unreachable!(),
        };
        self.top_free = val;
        Some(free)
    }

    pub fn lookup(&self, id: StringId) -> Option<&str> {
        match self.values.get(id.0.into_usize())? {
            InternValue::Present {
                ref string,
                count: _,
            } => Some(string.as_str()),
            _ => None,
        }
    }
}
