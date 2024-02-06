use core::fmt;
use std::num::NonZeroU32;

use ast::NodeId;
use common::{id::KeyedVec, key, string::StringId};

mod resolve;
pub use resolve::resolve_script;
mod render;

key!(
    #[derive(Ord,PartialOrd)]
    pub struct ScopeId(#[non_zero] NonZeroU32)
);
key!(pub struct SymbolId(#[non_zero] NonZeroU32));
key!(pub struct LoopId(#[non_zero] NonZeroU32));

impl ScopeId {
    fn next(self) -> Self {
        Self(self.0.checked_add(1).unwrap())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct SymbolUseOrder(NonZeroU32);

impl fmt::Display for SymbolUseOrder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (u32::from(self.0) - 1).fmt(f)
    }
}

impl SymbolUseOrder {
    pub fn first() -> SymbolUseOrder {
        SymbolUseOrder(NonZeroU32::new(1).unwrap())
    }

    pub fn last() -> SymbolUseOrder {
        SymbolUseOrder(NonZeroU32::new(u32::MAX).unwrap())
    }

    pub fn checked_add(self, v: u32) -> Option<Self> {
        self.0.checked_add(v).map(Self)
    }

    pub fn to_u32(self) -> u32 {
        u32::from(self.0) - 1
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub enum Kind {
    /// Defined with the side effect of introducing new global variables.
    Global,
    /// Define within the function scope.
    Function,
    /// Defined with let
    Let,
    /// Defined with const
    Const,
    /// An argument of a function
    Arg,
    /// A variable which could not be resolved to a declaration.
    Unresolved,
}

impl Kind {
    pub fn is_function_scoped(self) -> bool {
        matches!(self, Kind::Global | Kind::Function)
    }

    pub fn is_block_scoped(self) -> bool {
        matches!(self, Kind::Let | Kind::Const)
    }

    pub fn is_arg(self) -> bool {
        matches!(self, Kind::Arg)
    }
}

impl From<ast::VariableKind> for Kind {
    fn from(value: ast::VariableKind) -> Self {
        match value {
            ast::VariableKind::Const => Self::Const,
            ast::VariableKind::Var => Self::Function,
            ast::VariableKind::Let => Self::Let,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum LastUse {
    Unused,
    Direct(SymbolUseOrder),
    /// The variable is last used inside a child loop of it's declaration scope.
    Loop(LoopId),
}

#[derive(Clone, Copy, Debug)]
pub struct Symbol {
    /// The identifier of the variable.
    pub ident: StringId,
    /// Is the variable captured by a closure.
    pub captured: bool,
    /// How is the variable declared.
    pub kind: Kind,
    /// When the variable is first declared.
    pub declared: Option<SymbolUseOrder>,
    /// When the variable is defined.
    pub defined: Option<SymbolUseOrder>,
    /// When the variable is last used.
    pub last_use: LastUse,
    /// The scope the symbol was declared in.
    pub scope: ScopeId,
    /// A symbol in a super scope with a same name.
    pub shadows: Option<SymbolId>,
    pub ast_node: NodeId<ast::Symbol>,
}

#[derive(Clone, Copy, Debug)]
pub struct Scope {
    /// The parent of the scope
    parent: Option<ScopeId>,
    /// What type of kind
    kind: ScopeKind,
    /// How many children this scope has.
    num_scope_children: u32,
    /// The offset into the child array where you can find the childeren of this array.
    scope_child_offset: u32,
    /// The number of declarations.
    num_decl_children: u32,
    decl_child_offset: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ScopeKind {
    Function(NodeId<ast::Function>),
    Block { has_loop: bool },
    Static,
    Global { strict: bool },
}

impl ScopeKind {
    pub fn is_function_scope(&self) -> bool {
        matches!(self, ScopeKind::Function(_) | ScopeKind::Global { .. })
    }
}

#[derive(Clone, Copy, Debug)]
pub struct UseInfo {
    /// The use order of this use of a symbol
    pub use_order: SymbolUseOrder,
    /// The id of the symbol this is a use of
    pub id: Option<SymbolId>,
}

impl Default for UseInfo {
    fn default() -> Self {
        UseInfo {
            use_order: SymbolUseOrder::first(),
            id: None,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct LoopInfo {
    /// the parent loop of this loop.
    parent: Option<LoopId>,
    /// The scope to which this loop belongs.
    scope: ScopeId,
    /// The order assigned to this loop.
    /// All variables which are declared above this loop but are used inside have this use order.
    use_order: SymbolUseOrder,
}

#[derive(Clone, Debug)]
pub struct Variables {
    pub scopes: KeyedVec<ScopeId, Scope>,
    pub scope_children: Vec<ScopeId>,
    pub scope_decls: Vec<SymbolId>,
    pub symbols: KeyedVec<SymbolId, Symbol>,
    /// A list which maps a ast symbol to a resolved symbol.
    pub ast_to_symbol: KeyedVec<NodeId<ast::Symbol>, UseInfo>,
    pub loop_use: KeyedVec<LoopId, LoopInfo>,
}

impl Variables {
    pub fn new() -> Self {
        Variables {
            scopes: KeyedVec::new(),
            scope_children: Vec::new(),
            scope_decls: Vec::new(),
            symbols: KeyedVec::new(),
            ast_to_symbol: KeyedVec::new(),
            loop_use: KeyedVec::new(),
        }
    }

    pub fn push_global_scope(&mut self, strict: bool) -> ScopeId {
        // TODO: limits check.
        self.scopes.push(Scope {
            parent: None,
            kind: ScopeKind::Global { strict },
            num_scope_children: 0,
            scope_child_offset: 0,
            num_decl_children: 0,
            decl_child_offset: 0,
        })
    }

    pub fn symbol_of_ast(&self, ast: NodeId<ast::Symbol>) -> SymbolId {
        self.ast_to_symbol[ast]
            .id
            .expect("ast node was not properly resolved")
    }

    pub fn scopes(&self) -> &KeyedVec<ScopeId, Scope> {
        &self.scopes
    }

    pub fn symbols(&self) -> &KeyedVec<SymbolId, Symbol> {
        &self.symbols
    }

    pub fn child_scopes(&self, id: ScopeId) -> &[ScopeId] {
        let scope = &self.scopes[id];
        let offset = scope.scope_child_offset as usize;
        let len = scope.num_scope_children as usize;

        &self.scope_children[offset..(offset + len)]
    }

    pub fn declared_vars(&self, id: ScopeId) -> &[SymbolId] {
        let scope = &self.scopes[id];
        let offset = scope.decl_child_offset as usize;
        let len = scope.num_decl_children as usize;

        &self.scope_decls[offset..(offset + len)]
    }

    pub fn new_symbol(&mut self, origin: NodeId<ast::Symbol>, sym: Symbol) -> SymbolId {
        let id = self.symbols.push(sym);
        self.ast_to_symbol.insert_grow_default(
            origin,
            UseInfo {
                use_order: SymbolUseOrder::first(),
                id: Some(id),
            },
        );
        id
    }

    pub fn resolve_use(&mut self, origin: NodeId<ast::Symbol>, to: SymbolId) {
        self.ast_to_symbol.insert_grow_default(
            origin,
            UseInfo {
                use_order: SymbolUseOrder::first(),
                id: Some(to),
            },
        );
    }

    pub fn last_use_of(&self, symbol: SymbolId) -> Option<SymbolUseOrder> {
        match self.symbols[symbol].last_use {
            LastUse::Unused => None,
            LastUse::Direct(x) => Some(x),
            LastUse::Loop(l) => Some(self.loop_use[l].use_order),
        }
    }

    /// Returns the function like scope of this scope.
    ///
    /// This can be itself if the scope is already a function else it is the first
    /// parent scope which is either a function scope or the global scope.
    pub fn function_of(&self, mut scope: ScopeId) -> ScopeId {
        loop {
            if self.scopes[scope].kind.is_function_scope() {
                return scope;
            }
            scope = self.scopes[scope]
                .parent
                .expect("Found scope which wasn't a child of any function like scope");
        }
    }
}

impl Default for Variables {
    fn default() -> Self {
        Self::new()
    }
}
