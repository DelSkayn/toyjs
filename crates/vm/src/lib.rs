//! This library contains the vm and the runtime of the toyjs interpreter.
//!
//!
//! ### Safety
//!
//! The toyjs runtime is not written to be implemented in completely safe rust code.
//! This decision stems from the fact that a safe vm runtime would need to track and handle code
//! variants which should never happen in general use. This would both slow down the interpreter as
//! well as complicate the implementation,
//!
//! There are two primary assumptions the vm makes which causes code to be unsafe:
//!
//! 1. Bytecode being run in the vm is correct.
//! 2. Gc pointers are correctly traced.
//!
//! For further explanation of these assumptions see the [`instructions`] and [`gc`] modules respectively.
//!
//! All code assumes that these two statemets hold during exection. This makes a lot of the runtime
//! unsafe as violating any of these two assumptions will result in undefined behaviour,
//!

#![allow(dead_code)]
#![allow(clippy::missing_safety_doc)]
#![allow(clippy::new_without_default)]
#![feature(allocator_api)]

pub mod gc;
use std::{cell::Cell, marker::PhantomData};

pub use gc::{Gc, GcArena};
pub mod instructions;
pub mod value;
pub use value::Value;
pub mod object;
pub use object::Object;
mod function;
pub use function::Function;

pub mod realm;
pub use realm::Realm;

pub struct Context<'a> {
    vm: &'a mut Realm,
    marker: PhantomData<Cell<&'a ()>>,
}
