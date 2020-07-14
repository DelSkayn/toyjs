#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_macros)]

#[macro_use]
extern crate log;

#[macro_use]
mod macros;

pub mod ast;
pub mod compiler;
pub mod lexer;
pub mod parser;
pub mod runtime;
pub mod source;
pub mod token;