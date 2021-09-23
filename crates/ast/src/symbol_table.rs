#![allow(dead_code)]

use common::{
    interner::StringId,
    newtype_key,
    slotmap::{SlotKey, SlotMap},
};
use std::{
    alloc::{Allocator, Global},
    collections::HashMap,
};

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum DeclType {
    /// Declared with `var`
    Global,
    /// Declared implicitly by using a variable before it is declared.
    Implicit,
    /// Declared with `let`
    Local,
    /// Declared with `const`
    Const,
    /// Declared as a function argument
    Argument,
}

pub struct Symbol {
    decl_type: DeclType,
    decl_scope: ScopeId,
    ident: StringId,
}

newtype_key! {
    pub struct SymbolId(u32);
}
type Symbols<A> = SlotMap<Symbol, SymbolId, A>;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum ScopeKind {
    /// A scope created as the scope of a new function.
    Function,
    /// A scope created by a code block.
    Lexical,
    /// The Global scope of a script.
    Global,
    /// The scope of a module.
    Module,
}

/// A lexical scope.
pub struct Scope<A: Allocator> {
    parent: Option<ScopeId>,
    parent_function: Option<ScopeId>,
    symbols: Vec<SymbolId, A>,
    children: Vec<ScopeId, A>,
    kind: ScopeKind,
}

impl<A: Allocator + Clone> Scope<A> {
    pub fn new_in(
        parent: Option<ScopeId>,
        parent_function: Option<ScopeId>,
        kind: ScopeKind,
        alloc: A,
    ) -> Self {
        Scope {
            parent,
            parent_function,
            symbols: Vec::new_in(alloc.clone()),
            children: Vec::new_in(alloc),
            kind,
        }
    }
}

newtype_key! {
    pub struct ScopeId(u32);
}

type Scopes<A> = SlotMap<Scope<A>, ScopeId, A>;

pub struct SymbolTable<A: Allocator> {
    scopes: Scopes<A>,
    symbols: Symbols<A>,
    symbols_by_ident: HashMap<(ScopeId, StringId), SymbolId>,
    global: ScopeId,
    alloc: A,
}

impl SymbolTable<Global> {
    pub fn new() -> Self {
        Self::new_in(Global)
    }
}

impl<A: Allocator + Clone> SymbolTable<A> {
    pub fn new_in(alloc: A) -> Self {
        let mut scopes = Scopes::new_in(alloc.clone());
        let global = scopes.insert(Scope {
            parent: None,
            parent_function: None,
            symbols: Vec::new_in(alloc.clone()),
            children: Vec::new_in(alloc.clone()),
            kind: ScopeKind::Global,
        });
        SymbolTable {
            scopes,
            symbols: Symbols::new_in(alloc.clone()),
            symbols_by_ident: HashMap::new(),
            global,
            alloc,
        }
    }
}

impl<A: Allocator> SymbolTable<A> {
    /// Returns the map containing all symbols
    pub fn symbols(&self) -> &Symbols<A> {
        &self.symbols
    }

    /// Returns the map containing all scopes
    pub fn scopes(&self) -> &Scopes<A> {
        &self.scopes
    }
}

pub struct SymbolTableBuilder<'a, A: Allocator> {
    table: &'a mut SymbolTable<A>,
    current_scope: ScopeId,
    current_function: ScopeId,
}

impl<'a, A: Allocator + Clone> SymbolTableBuilder<'a, A> {
    /// Creates a new builder from a existing table.
    pub fn new_script(table: &'a mut SymbolTable<A>) -> Self {
        SymbolTableBuilder {
            current_scope: table.global,
            current_function: table.global,
            table,
        }
    }

    pub fn new_module(table: &'a mut SymbolTable<A>) -> Self {
        let new_root = table.scopes.insert(Scope {
            parent: None,
            parent_function: None,
            symbols: Vec::new_in(table.alloc.clone()),
            children: Vec::new_in(table.alloc.clone()),
            kind: ScopeKind::Module,
        });
        SymbolTableBuilder {
            current_scope: new_root,
            current_function: new_root,
            table,
        }
    }
    /// Looks up a symbol in current and parent scopes by the symbol's name.
    /// Returns None if no symbol with the given name was found in the current and parent scopes.
    fn lookup_symbol(&self, name: StringId) -> Option<SymbolId> {
        let mut cur_scope = self.current_scope;
        loop {
            if let Some(x) = self.table.symbols_by_ident.get(&(cur_scope, name)) {
                return Some(*x);
            }
            if let Some(x) = self.table.scopes[cur_scope].parent {
                cur_scope = x;
            } else {
                return None;
            }
        }
    }

    pub fn define(&mut self, name: StringId, kind: DeclType) -> Option<SymbolId> {
        match kind {
            DeclType::Local | DeclType::Const | DeclType::Argument => {
                if self
                    .table
                    .symbols_by_ident
                    .get(&(self.current_scope, name))
                    .is_some()
                {
                    // Redeclared in same scope
                    return None;
                }
            }
            DeclType::Global => {
                let existing = self
                    .table
                    .symbols_by_ident
                    .get(&(self.current_scope, name))
                    .copied()
                    .or_else(|| {
                        self.table
                            .symbols_by_ident
                            .get(&(self.current_function, name))
                            .copied()
                    });
                if let Some(existing) = existing {
                    match self.table.symbols[existing].decl_type {
                        // Global may be redeclared without issue
                        DeclType::Global => return Some(existing),
                        DeclType::Local | DeclType::Const | DeclType::Argument => return None,
                        DeclType::Implicit => {
                            // Value previously implicitly declared is now declared as a global.
                            self.table.symbols[existing].decl_type = DeclType::Global;
                            return Some(existing);
                        }
                    }
                }
            }
            DeclType::Implicit => panic!("can't define variables explicitly implicit."),
        }
        // Variable was not declared yeu.

        match kind {
            // Let and const's are declared in lexical scope.
            // NOTE: not entirely true to spec but the semantics are almost identical.
            DeclType::Local | DeclType::Const => {
                let new_symbol = self.table.symbols.insert(Symbol {
                    decl_type: kind,
                    decl_scope: self.current_scope,
                    ident: name,
                });
                self.table.scopes[self.current_scope]
                    .symbols
                    .push(new_symbol);
                return Some(new_symbol);
            }
            // Arguments and globals are always declared at function scope
            DeclType::Argument | DeclType::Global => {
                let new_symbol = self.table.symbols.insert(Symbol {
                    decl_type: kind,
                    decl_scope: self.current_function,
                    ident: name,
                });
                self.table.scopes[self.current_function]
                    .symbols
                    .push(new_symbol);
                return Some(new_symbol);
            }
            DeclType::Implicit => panic!("can't define variables explicitly implicit."),
        }
    }

    /// Use a symbol,
    /// Implicitly declares a variable if it was not yet declared.
    pub fn use_symbol(&mut self, name: StringId) -> SymbolId {
        if let Some(x) = self.lookup_symbol(name) {
            return x;
        }
        let new_symbol = self.table.symbols.insert(Symbol {
            decl_type: DeclType::Implicit,
            decl_scope: self.current_function,
            ident: name,
        });
        self.table.scopes[self.current_function]
            .symbols
            .push(new_symbol);
        return new_symbol;
    }

    /// Push a new scope onto the stack
    pub fn push_scope(&mut self, kind: ScopeKind) -> ScopeId {
        assert!(
            kind != ScopeKind::Global && kind != ScopeKind::Module,
            "can't push global and module scopes"
        );

        let new_scope = self.table.scopes.insert(Scope {
            kind,
            parent: Some(self.current_scope),
            parent_function: Some(self.current_function),
            symbols: Vec::new_in(self.table.alloc.clone()),
            children: Vec::new_in(self.table.alloc.clone()),
        });
        self.current_scope = new_scope;
        if kind == ScopeKind::Function {
            self.current_function = new_scope;
        }
        new_scope
    }

    /// Pop a scope from the stack.
    pub fn pop_scope(&mut self) -> ScopeId {
        let new_scope = self.table.scopes[self.current_scope]
            .parent
            .expect("tried to pop a scope without a parent");

        if self.current_scope == self.current_function {
            self.current_function = self.table.scopes[self.current_scope]
                .parent_function
                .expect("tried to pop root scope");
        }
        self.current_scope = new_scope;
        self.current_scope
    }

    pub fn current_scope(&self) -> ScopeId {
        self.current_scope
    }

    pub fn current_function_scope(&self) -> ScopeId {
        self.current_function
    }
}
