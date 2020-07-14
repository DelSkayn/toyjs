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

pub struct Interner {
    map: FxHashMap<&'static str, u32>,
    strings: Vec<&'static str>,
    buf: String,
    full: Vec<String>,
}

impl Interner {
    pub fn with_capacity(cap: usize) -> Self {
        let mut res = Interner {
            map: FxHashMap::default(),
            strings: Vec::new(),
            buf: String::with_capacity(cap),
            full: Vec::new(),
        };
        for (i, p) in consts::PRELOAD.iter().enumerate() {
            let id = res.intern(p);
            debug_assert_eq!(id.0, i as u32);
        }
        res
    }

    pub fn intern(&mut self, name: &str) -> StringId {
        if let Some(&id) = self.map.get(name) {
            return StringId(id);
        }
        let name = unsafe { self.alloc(name) };
        let id = self.map.len() as u32;
        self.map.insert(name, id);
        self.strings.push(name);

        StringId(id)
    }

    pub fn lookup(&self, id: StringId) -> &str {
        self.strings[id.0 as usize]
    }

    unsafe fn alloc(&mut self, name: &str) -> &'static str {
        let cap = self.buf.capacity();
        if cap < self.buf.len() + name.len() {
            let new_cap = (cap.max(name.len()) + 1).next_power_of_two();
            let new_buf = String::with_capacity(new_cap);
            let old_buf = mem::replace(&mut self.buf, new_buf);
            self.full.push(old_buf);
        }

        let interned = {
            let start = self.buf.len();
            self.buf.push_str(name);
            &self.buf[start..]
        };

        &*(interned as *const str)
    }
}
