use std::{cell::UnsafeCell, collections::BTreeMap};

use crate::{gc::Trace, Value};

enum ElementsInner {
    Array(Vec<Value>),
    Tree(BTreeMap<usize, Value>),
}

#[derive(Debug)]
pub struct Elements(UnsafeCell<ElementsInner>);

impl Elements {
    /// The ratio of empty values a entry must be from the previous in order to cause a fallback to a
    /// hashmap implementation
    const BACKDOWN_RATIO: f64 = 1.5;
    const BACKDOWN_MINIMUM: usize = 8;

    pub fn new() -> Self {
        Elements({
            let value = ElementsInner::Array(Vec::new());
            UnsafeCell { value }
        })
    }

    pub fn set(&self, key: usize, v: Value) {
        unsafe {
            // Safe as we only hold the mutable reference within this scope and no references can
            // escape.
            match *self.0.get() {
                ElementsInner::Array(ref mut array) => {
                    if key >= array.len() {
                        if key > Self::BACKDOWN_MINIMUM
                            && key > (array.len() as f64 * Self::BACKDOWN_RATIO).floor() as usize
                        {
                            let mut map: BTreeMap<usize, Value> = array
                                .iter()
                                .copied()
                                .filter(|x| !x.is_empty())
                                .enumerate()
                                .collect();
                            map.insert(key, v);
                            (*self.0.get()) = ElementsInner::Tree(map);
                            return;
                        }
                        array.resize(key, Value::empty());
                        array.push(v);
                    } else {
                        // Safe because key bound is checked above
                        *array.get_unchecked_mut(key) = v
                    }
                }
                ElementsInner::Tree(ref mut tree) => {
                    tree.insert(key, v);
                }
            }
        }
    }

    pub fn get(&self, key: usize) -> Option<Value> {
        unsafe {
            // Safe as we only hold the mutable reference within this scope and no references can
            // escape.
            match *self.0.get() {
                ElementsInner::Array(ref array) => {
                    array
                        .get(key)
                        .copied()
                        .and_then(|x| if x.is_empty() { None } else { Some(x) })
                }
                ElementsInner::Tree(ref tree) => tree.get(&key).copied(),
            }
        }
    }

    pub fn for_each<F: FnMut(usize, Value)>(&self, mut f: F) {
        unsafe {
            match *self.0.get() {
                ElementsInner::Array(ref array) => array
                    .iter()
                    .copied()
                    .enumerate()
                    .filter(|x| !x.1.is_empty())
                    .for_each(|(a, b)| f(a, b)),
                ElementsInner::Tree(ref t) => {
                    t.iter().map(|(a, b)| (*a, *b)).for_each(|(a, b)| f(a, b))
                }
            }
        }
    }
}

unsafe impl Trace for Elements {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        unsafe {
            // Safe as we only hold the mutable reference within this scope and no references can
            // escape.
            match *self.0.get() {
                ElementsInner::Array(ref x) => {
                    x.iter().for_each(|x| x.trace(ctx));
                }
                ElementsInner::Tree(ref x) => {
                    x.values().for_each(|x| x.trace(ctx));
                }
            }
        }
    }
}
