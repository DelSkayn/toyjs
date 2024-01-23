use ast::NodeId;
use common::{hashmap::hash_map::HashMap, id::KeyedVec, key, string::StringId};

mod resolve;
mod resolve2;
pub use resolve::VariablesResolver;
mod render;

key!(
#[derive(Ord,PartialOrd)]
pub struct ScopeId(u32)
);
key!(pub struct SymbolId(u32));

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct SymbolUseOrder(pub u32);

impl SymbolUseOrder {
    pub fn first() -> SymbolUseOrder {
        SymbolUseOrder(0)
    }

    pub fn last() -> SymbolUseOrder {
        SymbolUseOrder(u32::MAX)
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
    pub last_use: Option<SymbolUseOrder>,
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
    Block,
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
    pub use_order: SymbolUseOrder,
    pub id: Option<SymbolId>,
}

#[derive(Clone, Debug)]
pub struct Variables {
    pub scopes: KeyedVec<ScopeId, Scope>,
    pub scope_children: Vec<ScopeId>,
    pub scope_symbols: Vec<SymbolId>,
    pub symbols: KeyedVec<SymbolId, Symbol>,
    /// A list which maps a ast symbol to a resolved symbol.
    pub use_to_symbol: KeyedVec<NodeId<ast::Symbol>, UseInfo>,
}

impl Variables {
    pub fn new() -> Self {
        Variables {
            scopes: KeyedVec::new(),
            scope_children: Vec::new(),
            scope_symbols: Vec::new(),
            symbols: KeyedVec::new(),
            use_to_symbol: KeyedVec::new(),
        }
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

        &self.scope_symbols[offset..(offset + len)]
    }

    pub fn new_symbol(&mut self, origin: NodeId<ast::Symbol>, sym: Symbol) -> SymbolId {
        let id = self.symbols.push(sym);
        self.use_to_symbol.insert_grow(
            origin,
            UseInfo {
                use_order: SymbolUseOrder(0),
                id: Some(id),
            },
            UseInfo {
                use_order: SymbolUseOrder(0),
                id: None,
            },
        );
        id
    }

    pub fn resolve_use(&mut self, origin: NodeId<ast::Symbol>, to: SymbolId) {
        self.use_to_symbol.insert_grow(
            origin,
            UseInfo {
                use_order: SymbolUseOrder(0),
                id: Some(to),
            },
            UseInfo {
                use_order: SymbolUseOrder(0),
                id: None,
            },
        );
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
