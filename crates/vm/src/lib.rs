//! This library contains the vm and the runtime of the toyjs interpreter.
//!
//! ### Safety
//!
//! The toyjs vm contains lots and lots of unsafe code.
//! Overall we have found that it is very difficult to implement a interpreter in safe rust without
//! implementing lots of code just to check wether operations which should be safe actually are.
//!
//! For example: this VM implements a trace and sweep garbage collector. Inorder to trace a GC
//! needs to know what the root values are which indicate that a values is still alive.
//! In the interpreter these roots are pretty easy to find, there the global object and all the
//! values on currently on the stack. However we cannot guaruantee that those are the only values.
//! If we wanted to implement this GC completely safe we would need to keep track of all the roots,
//! Track when values are moved into value which is alive or out of a value which is alive track
//! when roots are dropped,etc. This would all be pretty intensive checking while instead we could
//! require that all the roots are present and handed to the gc when doing collection.
//!
//! So in order to avoid complicating design this crate is largely unsafe.

#![allow(dead_code)]
#![allow(clippy::missing_safety_doc)]
#![allow(clippy::new_without_default)]
#![feature(allocator_api)]

pub mod gc;

pub use gc::{Gc, GcArena};
pub mod instructions;
pub mod value;
pub use value::Value;
pub mod object;
pub use object::Object;

mod vm;
pub use vm::*;

pub mod realm;
pub use realm::Realm;

pub mod atom;
pub mod lock;
