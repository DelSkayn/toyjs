//! A library implementing common utitilies used throughout toyjs.

#![allow(dead_code)]

pub use hashbrown as hashmap;

pub mod interner;
pub mod source;
pub mod span;
pub mod string;
pub mod tagged_ptr;
pub mod unicode;
