//! A string interner
//!
//! based on the blog post: https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html

use fxhash::FxHashMap;
use std::mem;

/// Some preloaded strings in the interner often used
pub mod consts {
    use super::StringId;

    pub(crate) const PRELOAD: [&'static str; 7] =
        ["target", "get", "set", "static", "of", "import", "meta"];

    pub const TARGET: StringId = StringId(0);
    pub const GET: StringId = StringId(1);
    pub const SET: StringId = StringId(2);
    pub const STATIC: StringId = StringId(3);
    pub const OF: StringId = StringId(4);
    pub const IMPORT: StringId = StringId(5);
    pub const META: StringId = StringId(6);
}

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub struct StringId(u32);

pub enum InternValue {
    Present { string: String, count: usize },
    Free(u32),
}

pub struct Interner {
    map: FxHashMap<String, u32>,
    values: Vec<InternValue>,
    top_free: u32,
}

impl Interner {
    pub fn new() -> Self {
        let mut res = Interner {
            map: FxHashMap::default(),
            values: Vec::new(),
            top_free: u32::MAX,
        };
        for (i, p) in consts::PRELOAD.iter().enumerate() {
            let id = res.intern(p);
            debug_assert_eq!(id.0, i as u32);
        }
        res
    }

    fn preallocate(&mut self, name: &str) -> StringId {
        let idx = self.values.len();
        self.values.push(InternValue::Present {
            string: name.to_string(),
            count: usize::MAX,
        });
        self.map.insert(name.to_string(), idx as u32);
        StringId(idx as u32)
    }

    pub fn intern(&mut self, name: &str) -> StringId {
        if let Some(&id) = self.map.get(name) {
            return StringId(id);
        }
        let id = self.allocate(name.to_string());
        self.map.insert(name.to_string(), id);
        StringId(id)
    }

    fn allocate(&mut self, s: String) -> u32 {
        if let Some(x) = self.remove_free() {
            self.values[x as usize] = InternValue::Present {
                string: s,
                count: 1,
            };
            return x;
        }
        let res = self.values.len();
        assert!(res < u32::MAX as usize, "too many interned strings");
        self.values.push(InternValue::Present {
            string: s,
            count: 1,
        });
        return res as u32;
    }

    pub fn increment(&mut self, id: StringId) {
        match self.values[id.0 as usize] {
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
        match self.values[id.0 as usize] {
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

    fn insert_free(&mut self, idx: u32) {
        self.map.remove(match self.values[idx as usize] {
            InternValue::Present {
                ref string,
                count: _,
            } => string,
            _ => unreachable!(),
        });
        self.values[idx as usize] = InternValue::Free(self.top_free);
        self.top_free = idx;
    }

    fn remove_free(&mut self) -> Option<u32> {
        if self.top_free == u32::MAX {
            return None;
        }
        let val = match self.values[self.top_free as usize] {
            InternValue::Free(x) => x,
            _ => unreachable!(),
        };
        let res = self.top_free;
        self.top_free = val;
        return Some(res);
    }

    pub fn lookup(&self, id: StringId) -> Option<&str> {
        match self.values.get(id.0 as usize)? {
            InternValue::Present {
                ref string,
                count: _,
            } => Some(string.as_str()),
            _ => None,
        }
    }
}
