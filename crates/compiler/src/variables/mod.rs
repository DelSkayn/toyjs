use ast::NodeId;
use common::{id::KeyedVec, key, string::StringId};

mod builder;
pub use builder::VariablesBuilder;
mod render;

key!(pub struct ScopeId(u32));
key!(pub struct SymbolId(u32));

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct SymbolUseOrder(pub u32);

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
    ident: StringId,
    /// Is the variable captured by a closure.
    captured: bool,
    /// How is the variable declared.
    kind: Kind,
    /// When the variable is first declared.
    declared: Option<NodeId<ast::Symbol>>,
    /// When the variable is defined.
    defined: Option<NodeId<ast::Expr>>,
    /// When the variable is last used.
    last_use: Option<NodeId<ast::Expr>>,
    /// The scope the symbol was declared in.
    scope: ScopeId,
}

#[derive(Clone, Copy, Debug)]
pub struct Scope {
    /// The parent of the scope
    parent: Option<ScopeId>,
    /// What type of kind
    kind: ScopeKind,
    /// How many children this scope has.
    num_childeren: u32,
    /// The offset into the child array where you can find the childeren of this array.
    child_offset: u32,
    /// The number of declarations.
    num_declarations: u32,
    symbol_offset: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ScopeKind {
    Function(NodeId<ast::Function>),
    Block(NodeId<ast::Stmt>),
    Static(NodeId<ast::ClassMember>),
    Global { strict: bool },
}

impl ScopeKind {
    pub fn is_function_scope(&self) -> bool {
        matches!(self, ScopeKind::Function(_) | ScopeKind::Global { .. })
    }
}

#[derive(Clone, Copy, Debug)]
pub struct UseInfo {
    use_order: SymbolUseOrder,
    id: Option<SymbolId>,
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
        let offset = scope.child_offset as usize;
        let len = scope.num_childeren as usize;

        &self.scope_children[offset..(offset + len)]
    }

    pub fn declared_vars(&self, id: ScopeId) -> &[SymbolId] {
        let scope = &self.scopes[id];
        let offset = scope.symbol_offset as usize;
        let len = scope.num_declarations as usize;

        &self.scope_symbols[offset..(offset + len)]
    }

    /// Returns the function like scope of this scope.
    ///
    /// This can be itself if the scope is already a function like scope else it is the first
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
