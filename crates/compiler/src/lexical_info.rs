use ast::{ScopeId, SymbolId, SymbolTable};
use common::slotmap::SlotMap;
use std::alloc::Allocator;

pub enum SymbolInfo {
    Lexical { captured: Option<u16> },
    Global,
}

pub struct ScopeInfo {
    env_depth: Option<u16>,
    captured: Vec<u16>,
}

pub struct LexicalInfo<A: Allocator> {
    scope_info: SlotMap<ScopeInfo, ScopeId, A>,
    symbol_info: SlotMap<SymbolInfo, SymbolId, A>,
}

impl<A: Allocator> LexicalInfo<A> {
    pub fn new(_table: &SymbolTable<A>) -> Self {
        todo!()
    }
}
