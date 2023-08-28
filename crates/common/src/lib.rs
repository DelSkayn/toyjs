//! A library implementing common utitilies used throughout toyjs.

#![allow(dead_code)]

pub use hashbrown as hashmap;

mod mac;
pub use mac::*;

pub mod any_vec;
pub mod id;
pub mod interner;
pub mod number;
pub mod result;
pub mod source;
pub mod span;
pub mod string;
pub mod structs;
pub mod tagged_ptr;
pub mod unicode;
