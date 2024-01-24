#![allow(dead_code)]

use common::{
    interner::StringId,
    collections::HashMap,
    newtype_key,
    slotmap::{SlotKey, SlotVec},
};
use std::{
    alloc::{Allocator, Global},
    fmt, mem,
};

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum DeclType {
    /// Declared with `var`
    Var,
    /// Declared implicitly by using a variable before it is declared.
    /// Could be resolved later to a valid decltype
    Implicit,
    /// Implicitly declared without ever being defined
    /// Forbidden in strict mode.
    ImplicitGlobal,
    /// Declared with `let`
    Let,
    /// Declared with `const`
    Const,
    /// Declared as a function argument
    Argument,
}

impl DeclType {
    pub fn is_always_local(self) -> bool {
        match self {
            DeclType::Let | DeclType::Const | DeclType::Argument => true,
            DeclType::Var | DeclType::Implicit | DeclType::ImplicitGlobal => false,
        }
    }
}

/// Data about a lexical symbol
#[derive(Debug)]
pub struct Symbol {
    /// type of symbol
    pub decl_type: DeclType,
    /// The id of the scope this symbol was declared in.
    pub decl_scope: ScopeId,
    /// The identifier with which this identifier was declared.
    pub ident: StringId,
}

newtype_key! {
    /// A identifier of a certain symbol.
    pub struct SymbolId(u32);
}
type Symbols<A> = SlotVec<Symbol, SymbolId, A>;

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
    /// The parent of the scope
    pub parent: Option<ScopeId>,
    /// The function scope in which this scope was declared.
    pub parent_function: Option<ScopeId>,
    /// The symbols declared in this scope.
    pub symbols: Vec<SymbolId, A>,
    /// The children scopes of this scope.
    pub children: Vec<ScopeId, A>,
    /// The kind of scope.
    pub kind: ScopeKind,
    /// The depth of the scope, i.e. number of parent function scopes
    pub function_depth: u32,
}

impl<A: Allocator> fmt::Debug for Scope<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Scope")
            .field("parent", &self.parent)
            .field("parent_function", &self.parent_function)
            .field("symbols", &self.symbols)
            .field("children", &self.children)
            .field("kind", &self.kind)
            .field("function_depth", &self.function_depth)
            .finish()
    }
}

impl<A: Allocator + Clone> Scope<A> {
    pub fn new_in(
        parent: Option<ScopeId>,
        parent_function: Option<ScopeId>,
        kind: ScopeKind,
        depth: u32,
        alloc: A,
    ) -> Self {
        Scope {
            parent,
            parent_function,
            symbols: Vec::new_in(alloc.clone()),
            children: Vec::new_in(alloc),
            function_depth: depth,
            kind,
        }
    }
}

newtype_key! {
    /// A identifier of a certain scope.
    pub struct ScopeId(u32);
}

impl ScopeId {
    pub const fn root() -> Self {
        ScopeId(0)
    }
}

type Scopes<A> = SlotVec<Scope<A>, ScopeId, A>;

pub struct SymbolTable<A: Allocator> {
    scopes: Scopes<A>,
    symbols: Symbols<A>,
    /// Used for symbol lookup.
    symbols_by_ident: HashMap<StringId, Vec<(ScopeId, SymbolId)>>,
    alloc: A,
}

impl<A: Allocator> fmt::Debug for SymbolTable<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SymbolTable")
            .field("scopes", &self.scopes)
            .field("symbols", &self.symbols)
            .finish()
    }
}

impl Default for SymbolTable<Global> {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable<Global> {
    pub fn new() -> Self {
        Self::new_in(Global)
    }
}

impl<A: Allocator + Clone> SymbolTable<A> {
    pub fn new_in(alloc: A) -> Self {
        let mut scopes = Scopes::new_in(alloc.clone());
        let global = scopes.insert(Scope::new_in(
            None,
            None,
            ScopeKind::Global,
            0,
            alloc.clone(),
        ));
        debug_assert_eq!(global, ScopeId::root());
        SymbolTable {
            scopes,
            symbols: Symbols::new_in(alloc.clone()),
            symbols_by_ident: HashMap::default(),
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

    pub fn is_symbol_local(&self, symbol: SymbolId) -> bool {
        let sym = &self.symbols[symbol];
        if sym.decl_type.is_always_local() {
            true
        } else {
            sym.decl_type == DeclType::Var && self.function_scope(sym.decl_scope) != ScopeId::root()
        }
    }

    /// Returns wether a scope is the child of an other scope directly or indirectly.
    pub fn child_of(&self, mut child: ScopeId, parent: ScopeId) -> bool {
        while let Some(p) = self.scopes[child].parent {
            if p == parent {
                return true;
            }
            child = p;
        }
        false
    }

    /// Returns wether a symbol is in scope in the given scope.
    pub fn in_scope(&self, symbol: SymbolId, scope: ScopeId) -> bool {
        let symbol_scope = self.symbols[symbol].decl_scope;
        if symbol_scope == scope {
            return true;
        }
        self.child_of(symbol_scope, scope)
    }

    /// Travese the given scope and all its children in prefix order.
    pub fn traverse_scopes<F: FnMut(ScopeId, &Scope<A>)>(&self, scope: ScopeId, f: &mut F) {
        f(scope, &self.scopes[scope]);
        for child in &self.scopes[scope].children {
            self.traverse_scopes(*child, f);
        }
    }

    /// Returns the function scope within which this scope is declared.
    /// If the scope is a function scope it will return the current scope.
    pub fn function_scope(&self, scope: ScopeId) -> ScopeId {
        match self.scopes[scope].kind {
            ScopeKind::Function | ScopeKind::Module | ScopeKind::Global => scope,
            ScopeKind::Lexical => self.scopes[scope]
                .parent_function
                .expect("lexical scopes should always have a parent"),
        }
    }
}

pub struct SymbolTableBuilder<'a, A: Allocator> {
    table: &'a mut SymbolTable<A>,
    current_scope: ScopeId,
    current_function: ScopeId,
    current_depth: u32,
    pending_implicits: HashMap<SymbolId, Option<SymbolId>>,
}

impl<'a, A: Allocator> std::ops::Deref for SymbolTableBuilder<'a, A> {
    type Target = SymbolTable<A>;

    fn deref(&self) -> &Self::Target {
        self.table
    }
}

impl<'a, A: Allocator> std::ops::DerefMut for SymbolTableBuilder<'a, A> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.table
    }
}

impl<'a, A: Allocator + Clone> SymbolTableBuilder<'a, A> {
    /// Creates a new builder from a existing table.
    pub fn new_script(table: &'a mut SymbolTable<A>) -> Self {
        SymbolTableBuilder {
            current_scope: ScopeId::root(),
            current_function: ScopeId::root(),
            current_depth: 0,
            table,
            pending_implicits: HashMap::default(),
        }
    }

    pub fn new_module(table: &'a mut SymbolTable<A>) -> Self {
        let new_root = table.scopes.insert(Scope::new_in(
            None,
            None,
            ScopeKind::Module,
            0,
            table.alloc.clone(),
        ));
        SymbolTableBuilder {
            current_scope: new_root,
            current_function: new_root,
            current_depth: 0,
            table,
            pending_implicits: HashMap::default(),
        }
    }

    /// Looks up a symbol in current and parent scopes by the symbol's name.
    /// Returns None if no symbol with the given name was found in the current and parent scopes.
    fn lookup_symbol(&self, name: StringId) -> Option<SymbolId> {
        let mut cur_scope = self.current_scope;
        loop {
            if let Some(x) = self.table.symbols_by_ident.get(&name) {
                return x
                    .iter()
                    .rev()
                    .find(|x| x.0 == cur_scope || self.table.child_of(cur_scope, x.0))
                    .map(|x| x.1);
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
            DeclType::Let | DeclType::Const | DeclType::Argument => {
                if self
                    .table
                    .symbols_by_ident
                    .get(&name)
                    .map_or(false, |x| x.iter().any(|x| x.0 == self.current_scope))
                {
                    // Redeclared in same scope
                    return None;
                }
            }
            DeclType::Var => {
                let existing = self.table.symbols_by_ident.get(&name).and_then(|x| {
                    x.iter()
                        .find(|x| x.0 == self.current_scope || x.0 == self.current_function)
                        .map(|x| x.1)
                });
                if let Some(existing) = existing {
                    match self.table.symbols[existing].decl_type {
                        // Global may be redeclared without issue
                        DeclType::Var => return Some(existing),
                        DeclType::Let | DeclType::Const | DeclType::Argument => return None,
                        DeclType::Implicit => {
                            if self.table.symbols[existing].decl_scope == self.current_function {
                                // Value previously implicitly declared is now declared as a var.
                                self.table.symbols[existing].decl_type = DeclType::Var;
                                self.table.scopes[self.current_function]
                                    .symbols
                                    .push(existing);
                                self.pending_implicits.remove(&existing);
                                return Some(existing);
                            }
                        }
                        DeclType::ImplicitGlobal => {
                            if self.current_function == ScopeId::root() {
                                // Value previously implicitly declared is now declared as a var.
                                self.table.symbols[existing].decl_type = DeclType::Var;
                                self.table.scopes[self.current_function]
                                    .symbols
                                    .push(existing);
                                return Some(existing);
                            }
                        }
                    }
                }
            }
            DeclType::Implicit | DeclType::ImplicitGlobal => {
                panic!("can't define variables explicitly implicit.")
            }
        }

        // Variable was not declared yet.
        match kind {
            // Let and const's are declared in lexical scope.
            // NOTE: not entirely true to spec but the semantics are almost identical.
            DeclType::Let | DeclType::Const => {
                let new_symbol = self.table.symbols.insert(Symbol {
                    decl_type: kind,
                    decl_scope: self.current_scope,
                    ident: name,
                });
                self.table.scopes[self.current_scope]
                    .symbols
                    .push(new_symbol);
                self.table
                    .symbols_by_ident
                    .entry(name)
                    .or_insert_with(Vec::new)
                    .push((self.current_scope, new_symbol));
                Some(new_symbol)
            }
            // Arguments and globals are always declared at function scope
            DeclType::Argument | DeclType::Var => {
                let new_symbol = self.table.symbols.insert(Symbol {
                    decl_type: kind,
                    decl_scope: self.current_function,
                    ident: name,
                });
                self.table.scopes[self.current_function]
                    .symbols
                    .push(new_symbol);
                self.table
                    .symbols_by_ident
                    .entry(name)
                    .or_insert_with(Vec::new)
                    .push((self.current_function, new_symbol));
                Some(new_symbol)
            }
            DeclType::Implicit | DeclType::ImplicitGlobal => {
                panic!("can't define variables explicitly implicit.")
            }
        }
    }

    pub fn resolve_symbol(&mut self, symbol: SymbolId) -> SymbolId {
        // Symbol is already resolved in first pass
        if self.table.symbols[symbol].decl_type != DeclType::Implicit {
            return symbol;
        }
        // Symbol is already resolved in previous resolve_symbol call
        if let Some(x) = self.pending_implicits.get(&symbol).unwrap() {
            return *x;
        }

        let mut raise_symbols = vec![symbol];
        // Resolve the symbol.
        let symbol = &self.table.symbols[symbol];

        let symbols_with_name = self.table.symbols_by_ident.get(&symbol.ident).unwrap();

        let mut cur_scope = symbol.decl_scope;
        while let Some(parent) = self.table.scopes[cur_scope].parent_function {
            let find = symbols_with_name.iter().find(|x| x.0 == parent);
            if let Some((_, found_symbol)) = find {
                match self.table.symbols[*found_symbol].decl_type {
                    DeclType::Var | DeclType::Const | DeclType::Let | DeclType::ImplicitGlobal => {
                        for s in raise_symbols {
                            *self.pending_implicits.get_mut(&s).unwrap() = Some(*found_symbol);
                        }
                        return *found_symbol;
                    }
                    DeclType::Implicit => {
                        raise_symbols.push(*found_symbol);
                    }
                    DeclType::Argument => {
                        panic!("an implicity should never be able to be raised to an argument");
                    }
                }
            }
            cur_scope = parent;
        }

        let ident = symbol.ident;

        // No symbol found,
        // Variable is an implicit global
        let new_symbol = self.table.symbols.insert(Symbol {
            decl_type: DeclType::ImplicitGlobal,
            decl_scope: ScopeId::root(),
            ident,
        });

        self.table
            .symbols_by_ident
            .get_mut(&ident)
            .unwrap()
            .push((ScopeId::root(), new_symbol));

        for s in raise_symbols {
            *self.pending_implicits.get_mut(&s).unwrap() = Some(new_symbol);
        }

        new_symbol
    }

    /// Use a symbol,
    /// Implicitly declares a variable if it was not yet declared.
    pub fn use_symbol(&mut self, name: StringId) -> SymbolId {
        if let Some(x) = self.lookup_symbol(name) {
            let symbol = &self.table.symbols[x];
            if symbol.decl_type != DeclType::Implicit || symbol.decl_scope == self.current_function
            {
                return x;
            }
        }

        let new_symbol = self.table.symbols.insert(Symbol {
            decl_type: DeclType::Implicit,
            decl_scope: self.current_function,
            ident: name,
        });

        self.table
            .symbols_by_ident
            .entry(name)
            .or_insert_with(Vec::new)
            .push((self.current_function, new_symbol));

        self.pending_implicits.insert(new_symbol, None);

        new_symbol
    }

    /// Redefines a symbol used in a an arrow function argument as a arguments.
    pub fn reparse_function_param(&mut self, id: SymbolId) -> SymbolId {
        let symbol = &mut self.table.symbols[id];
        if let DeclType::Implicit = symbol.decl_type {
            symbol.decl_type = DeclType::Argument;
            let ident = symbol.ident;
            let old_scope = mem::replace(&mut symbol.decl_scope, self.current_function);
            let scopes = self.table.symbols_by_ident.get_mut(&ident).unwrap();
            for (ref mut scope, _) in scopes.iter_mut() {
                if *scope == old_scope {
                    *scope = self.current_function;
                    break;
                }
            }
            id
        } else {
            let ident = symbol.ident;
            self.define(ident, DeclType::Argument).unwrap()
        }
    }

    /// Push a new scope onto the stack
    pub fn push_scope(&mut self, kind: ScopeKind) -> ScopeId {
        assert!(
            kind != ScopeKind::Global && kind != ScopeKind::Module,
            "can't push global and module scopes"
        );

        if kind == ScopeKind::Function {
            self.current_depth += 1;
        }
        let new_scope = self.table.scopes.insert(Scope::new_in(
            Some(self.current_scope),
            Some(self.current_function),
            kind,
            self.current_depth,
            self.table.alloc.clone(),
        ));
        self.table.scopes[self.current_scope]
            .children
            .push(new_scope);
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
            self.current_depth -= 1;
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
