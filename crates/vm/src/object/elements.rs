use std::collections::BTreeMap;

use crate::{
    gc::{Trace, Tracer},
    Value,
};

enum ElementsInner<'gc, 'cell> {
    Array(Vec<Value<'gc, 'cell>>),
    Tree {
        tree: BTreeMap<usize, Value<'gc, 'cell>>,
        len: usize,
    },
}

pub struct Elements<'gc, 'cell>(ElementsInner<'gc, 'cell>);

impl<'gc, 'cell> Elements<'gc, 'cell> {
    /// The ratio of empty values a entry must be from the previous in order to cause a fallback to a
    /// hashmap implementation
    const BACKDOWN_RATIO: f64 = 1.5;
    const BACKDOWN_MINIMUM: usize = 8;

    pub fn new() -> Self {
        Elements(ElementsInner::Array(Vec::new()))
    }

    pub fn len(&self) -> usize {
        match self.0 {
            ElementsInner::Array(ref x) => x.len(),
            ElementsInner::Tree { len, .. } => len,
        }
    }

    pub fn set_len(&mut self, new_len: usize) {
        match self.0 {
            ElementsInner::Tree {
                ref mut tree,
                ref mut len,
            } => {
                if new_len < *len {
                    tree.split_off(&new_len);
                }
                *len = new_len;
            }
            ElementsInner::Array(ref mut array) => {
                if new_len > Self::BACKDOWN_MINIMUM
                    && new_len > (array.len() as f64 * Self::BACKDOWN_RATIO).floor() as usize
                {
                    let tree: BTreeMap<usize, Value> = array
                        .iter()
                        .copied()
                        .filter(|x| !x.is_empty())
                        .enumerate()
                        .collect();

                    self.0 = ElementsInner::Tree { tree, len: new_len };
                    return;
                }
                array.resize(new_len, Value::empty());
            }
        }
    }

    pub fn set(&mut self, key: usize, v: Value<'gc, 'cell>) {
        unsafe {
            // Safe as we only hold the mutable reference within this scope and no references can
            // escape.
            match self.0 {
                ElementsInner::Array(ref mut array) => {
                    if key >= array.len() {
                        if key > Self::BACKDOWN_MINIMUM
                            && key > (array.len() as f64 * Self::BACKDOWN_RATIO).floor() as usize
                        {
                            let mut tree: BTreeMap<usize, Value> = array
                                .iter()
                                .copied()
                                .filter(|x| !x.is_empty())
                                .enumerate()
                                .collect();

                            tree.insert(key, v);
                            self.0 = ElementsInner::Tree { tree, len: key };
                            return;
                        }
                        array.resize(key, Value::empty());
                        array.push(v);
                    } else {
                        // Safe because key bound is checked above
                        *array.get_unchecked_mut(key) = v
                    }
                }
                ElementsInner::Tree {
                    ref mut tree,
                    ref mut len,
                } => {
                    tree.insert(key, v);
                    *len = (*len).max(key);
                }
            }
        }
    }

    pub fn get(&self, key: usize) -> Option<Value<'gc, 'cell>> {
        // Safe as we only hold the mutable reference within this scope and no references can
        // escape.
        match self.0 {
            ElementsInner::Array(ref array) => {
                array
                    .get(key)
                    .copied()
                    .and_then(|x| if x.is_empty() { None } else { Some(x) })
            }
            ElementsInner::Tree { ref tree, .. } => tree.get(&key).copied(),
        }
    }

    pub fn for_each<F: FnMut(usize, Value)>(&self, mut f: F) {
        match self.0 {
            ElementsInner::Array(ref array) => array
                .iter()
                .copied()
                .enumerate()
                .filter(|x| !x.1.is_empty())
                .for_each(|(a, b)| f(a, b)),
            ElementsInner::Tree { ref tree, .. } => tree
                .iter()
                .map(|(a, b)| (*a, *b))
                .for_each(|(a, b)| f(a, b)),
        }
    }
}

unsafe impl<'gc, 'cell> Trace for Elements<'gc, 'cell> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, trace: Tracer) {
        // Safe as we only hold the mutable reference within this scope and no references can
        // escape.
        match self.0 {
            ElementsInner::Array(ref x) => {
                x.iter().for_each(|x| x.trace(trace));
            }
            ElementsInner::Tree { ref tree, .. } => {
                tree.values().for_each(|x| x.trace(trace));
            }
        }
    }
}
