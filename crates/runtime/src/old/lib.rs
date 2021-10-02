#![allow(unused)]

pub const MAX_REGISTERS: u8 = 255;

pub mod gc;
#[macro_use]
mod macros;
pub mod bytecode;
pub mod environment;
pub mod exec;
pub mod function;
pub mod object;
pub mod stack;
pub mod value;

use bytecode::RuntimeFunction;
use environment::Environment;
use exec::ExecutionContext;
use function::Function;
use gc::{Ctx, Gc, GcArena, Trace};
use object::Object;
use stack::Stack;
pub use std::{
    cell::RefCell,
    sync::{Arc, Mutex, Weak},
};
pub use value::JSValue;
