//! Javascript runtime object functionality

use crate::runtime::{rc::Rc, JSValue};
use fxhash::FxHashMap;
use std::hash;

pub type ObjectRc = Rc<Object>;

#[derive(Debug, Eq, PartialEq)]
pub struct Object {
    prototype: JSValue,
    map: FxHashMap<String, JSValue>,
}

impl hash::Hash for Object {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        for (k, v) in self.map.iter() {
            k.hash(state);
            v.hash(state);
        }
    }
}

impl Clone for Object {
    fn clone(&self) -> Self {
        let mut map = self.map.clone();
        for v in map.values_mut() {
            *v = unsafe { v.deep_clone() };
        }
        Object {
            prototype: unsafe { self.prototype.deep_clone() },
            map,
        }
    }
}

impl Object {
    pub fn new() -> Self {
        Object {
            prototype: JSValue::null(),
            map: FxHashMap::default(),
        }
    }

    pub fn get(&self, name: &str) -> JSValue {
        if let Some(x) = self.map.get(name) {
            return *x;
        }
        JSValue::undefined()
    }

    pub fn set(&mut self, name: String, value: JSValue) -> Option<JSValue> {
        if value.is_undefined() {
            return self.map.remove(&name);
        }
        self.map.insert(name, value)
    }
}
