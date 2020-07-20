#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_macros)]

#[macro_use]
mod macros;
#[macro_use]
extern crate log;

pub mod compiler;
pub mod interner;
pub mod lexer;
pub mod parser;
pub mod runtime;
pub mod source;
pub mod ssa;
pub mod token;
