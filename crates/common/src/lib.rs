//! A library implementing common utitilies used throughout toyjs.

#![allow(dead_code)]

pub mod hasher {
    pub use ahash::AHasher as Hasher;
}
pub mod hashmap {
    use core::hash::BuildHasherDefault;

    pub use hashbrown::hash_map;
    use hashbrown::{HashMap as BrownMap, HashSet as BrownSet};

    pub type HashMap<K, V> = BrownMap<K, V, BuildHasherDefault<ahash::AHasher>>;

    pub type HashSet<K> = BrownSet<K, BuildHasherDefault<ahash::AHasher>>;
}

pub mod id;
pub mod number;
pub mod result;
//pub mod smaller_vec;
#[macro_use]
pub mod assert;
pub mod source;
pub mod span;
pub mod string;
pub mod tagged_ptr;
pub mod unicode;
