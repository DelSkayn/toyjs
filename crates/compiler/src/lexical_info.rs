use ast::{
    symbol_table::{DeclType, Scope},
    ScopeId, SymbolId, SymbolTable,
};
use common::slotmap::SlotMap;
use std::{alloc::Allocator, convert::TryInto, fmt};

#[derive(Debug)]
pub enum SymbolInfo {
    Argument,
    Local,
    Captured(u16),
    Global,
}

pub type ScopeInfoMap<A> = SlotMap<ScopeInfo<A>, ScopeId, A>;
pub type SymbolInfoMap<A> = SlotMap<SymbolInfo, SymbolId, A>;

pub struct ScopeInfo<A: Allocator> {
    env_depth: Option<u16>,
    captured: Vec<ScopeId, A>,
}

impl<A: Allocator> fmt::Debug for ScopeInfo<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ScopeInfo")
            .field("env_depth", &self.env_depth)
            .field("captured", &self.captured)
            .finish()
    }
}

pub struct LexicalInfo<A: Allocator> {
    scope_info: ScopeInfoMap<A>,
    symbol_info: SymbolInfoMap<A>,
}

impl<A: Allocator + Clone> LexicalInfo<A> {
    pub fn new_in(root: ScopeId, table: &SymbolTable<A>, alloc: A) -> Self {
        let mut scope_info = SlotMap::new_in(alloc.clone());
        let mut symbol_info = SlotMap::new_in(alloc.clone());

        for global_symbol in table.scopes()[table.global()].symbols.iter().copied() {
            match table.symbols()[global_symbol].decl_type {
                DeclType::Global | DeclType::Implicit => {
                    symbol_info.insert(global_symbol, SymbolInfo::Global)
                }
                _ => {}
            }
        }

        // Calculate captured variables.
        table.traverse_scopes(root, &mut |id, scope| {
            scope_info.insert(
                id,
                ScopeInfo {
                    env_depth: None,
                    captured: Vec::new_in(alloc.clone()),
                },
            );

            for s in scope.symbols.iter().copied() {
                if symbol_info.get(s).is_none() {
                    symbol_info.insert(s, SymbolInfo::Local);
                }
            }

            for used in scope.used.iter().copied() {
                match symbol_info.get(used) {
                    Some(SymbolInfo::Global)
                    | Some(SymbolInfo::Captured(_))
                    | Some(SymbolInfo::Argument) => continue,
                    _ => {}
                }
                match table.symbols()[used].decl_type {
                    // Implicits are always globals.
                    DeclType::Implicit => symbol_info.insert(used, SymbolInfo::Global),
                    DeclType::Const | DeclType::Local | DeclType::Global => {
                        let decl_scope = table.symbols()[used].decl_scope;
                        let decl_function = table.function_scope(decl_scope);
                        // Variable is captured
                        let depth = table.scopes()[decl_function].function_depth;
                        // TODO: See if this unwrap can cause problems
                        // We're casting a u32 to u16 look into changing the u32 to a u16?
                        symbol_info.insert(used, SymbolInfo::Captured(depth.try_into().unwrap()));

                        // Variable in function scope is captured so it needs an environment.
                        scope_info[decl_function].env_depth = Some(0);

                        //Add the environment of the captured
                        if !scope_info[id].captured.contains(&decl_function) {
                            scope_info[id].captured.push(decl_function);
                        }
                    }
                    DeclType::Argument => {
                        symbol_info.insert(used, SymbolInfo::Argument);
                    }
                }
            }
        });

        // Calculate scope environment depth
        table.traverse_scopes(root, &mut |id, scope| {
            let info = &scope_info[id];
            if info.env_depth.is_some() {
                let mut cur = scope.parent_function;
                while let Some(x) = cur {
                    if let Some(x) = scope_info[x].env_depth {
                        scope_info[id].env_depth = Some(x + 1);
                        return;
                    }
                    cur = table.scopes()[x].parent_function;
                }
            }
        });
        Self {
            scope_info,
            symbol_info,
        }
    }

    pub fn symbol_info(&self) -> &SymbolInfoMap<A> {
        &self.symbol_info
    }

    pub fn scope_info(&self) -> &ScopeInfoMap<A> {
        &self.scope_info
    }
}

impl<A: Allocator> fmt::Debug for LexicalInfo<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LexicalInfo")
            .field("scope_info", &self.scope_info)
            .field("symbol_info", &self.symbol_info)
            .finish()
    }
}
