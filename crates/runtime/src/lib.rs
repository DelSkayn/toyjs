#![allow(dead_code)]
#![feature(allocator_api)]

pub mod gc;
pub use gc::{Gc, GcArena};
pub mod instructions;
pub mod value;
pub use value::JSValue;
pub mod object;
use object::Object;
mod function;
pub mod stack;

pub mod realm;
