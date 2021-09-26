#![allow(dead_code, unused_imports)]
#![feature(allocator_api)]

use ast::{Script, SymbolTable};
use runtime::bytecode::Bytecode;

use std::alloc::Allocator;

pub struct Compiler<'a, A: Allocator> {
    symbol_table: &'a SymbolTable<A>,
}

impl<'a, A: Allocator> Compiler<'a, A> {
    pub fn new(symbol_table: &'a SymbolTable<A>) -> Self {
        Compiler { symbol_table }
    }

    pub fn compile_script(&self, script: &Script<A>) -> Bytecode {}
}
