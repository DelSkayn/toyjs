use std::cell::UnsafeCell;

use common::collections::HashSet;

use crate::{atom::Atom, gc::Trace, Gc, Object, Realm, Value};

use super::{Property, PropertyFlags};

pub struct ForInIterator(Box<UnsafeCell<ForInIteratorInner>>);

struct ForInIteratorInner {
    visited_keys: HashSet<Atom>,
    visited_objects: HashSet<*mut ()>,
    remaining: Vec<Property>,
    cur_object: Option<Gc<Object>>,
}

unsafe impl Trace for ForInIterator {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        unsafe {
            (*self.0.get()).cur_object.trace(ctx);
        }
    }
}

impl ForInIterator {
    pub fn new(object: Gc<Object>) -> Self {
        Self(Box::new(UnsafeCell::new(ForInIteratorInner {
            visited_keys: HashSet::default(),
            visited_objects: HashSet::default(),
            remaining: object.properties.clone_properties(),
            cur_object: Some(object),
        })))
    }

    pub fn next(&self, realm: &Realm) -> Option<Value> {
        unsafe {
            let inner = &mut (*self.0.get());
            loop {
                if let Some(obj) = inner.cur_object {
                    while let Some(a) = inner.remaining.pop() {
                        if inner.visited_keys.contains(&a.key) {
                            continue;
                        }
                        if !a.flags.contains(PropertyFlags::ENUMERABLE) {
                            continue;
                        }
                        return Some(realm.atom_to_value(a.key));
                    }
                    inner.visited_objects.insert(Gc::into_raw(obj));
                    if let Some(proto) = obj.prototype {
                        if !inner.visited_objects.contains(&Gc::into_raw(proto)) {
                            inner.cur_object = Some(proto);
                            inner.remaining = proto.properties.clone_properties();
                        }
                    } else {
                        inner.cur_object = None;
                    }
                } else {
                    return None;
                }
            }
        }
    }
}

impl Gc<Object> {
    pub fn next(&self, realm: &Realm) -> Option<Value> {
        match self.kind {
            super::ObjectKind::ForInIterator(ref x) => x.next(realm),
            _ => None,
        }
    }
}
