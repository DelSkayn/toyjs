//! A library implementing common utitilies used throughout toyjs.

#![allow(dead_code)]

pub mod hashmap {
    use core::hash::BuildHasherDefault;
    pub use hashbrown::hash_map;
    use hashbrown::HashMap as BrownMap;

    pub type HashMap<K, V> = BrownMap<K, V, BuildHasherDefault<ahash::AHasher>>;
}

mod mac;
pub use mac::*;

pub mod any_vec;
pub mod id;
pub mod interner;
pub mod number;
pub mod result;
//pub mod smaller_vec;
pub mod source;
pub mod span;
pub mod string;
pub mod structs;
pub mod tagged_ptr;
pub mod unicode;
