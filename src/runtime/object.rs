use crate::runtime::{rc::Rc, JSValue};
use fxhash::FxHashMap;

pub type ObjectRc = Rc<Object>;

#[derive(Debug)]
pub struct Object {
    map: FxHashMap<String, JSValue>,
}

impl Clone for Object {
    fn clone(&self) -> Self {
        let mut map = self.map.clone();
        for v in map.values_mut() {
            *v = unsafe { v.clone() };
        }
        Object { map }
    }
}

impl Object {
    pub fn new() -> Self {
        Object {
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
            self.map.remove(&name);
            return None;
        }
        self.map.insert(name, value)
    }
}
