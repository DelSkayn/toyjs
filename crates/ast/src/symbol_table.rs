#![allow(dead_code)]

use std::alloc::Allocator;

pub struct SymbolTable<A: Allocator> {
    allocator: A,
}
