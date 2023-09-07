use ast::{Ast, ListHead, NodeId};
use common::{id, id::KeyedVec, string::StringId};

use crate::{Error, Limits};

use super::Result;

mod expr;
mod render;
mod stmt;

id!(pub struct ScopeId(u32));
id!(pub struct SymbolId(u32));

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
    /// Variable is used without ever being declared.
    Unresolved,
    Arg,
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
    declared: Option<NodeId<ast::IdentOrPattern>>,
    /// When the variable is defined.
    defined: Option<NodeId<ast::Expr>>,
    /// When the variable is last used.
    last_use: Option<NodeId<ast::Expr>>,
}

#[derive(Clone, Copy, Debug)]
pub struct Scope {
    parent: Option<ScopeId>,
    kind: ScopeKind,
    num_childeren: u32,
    child_list_offset: u32,
    num_declarations: u32,
    symbol_offset: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ScopeKind {
    Function(NodeId<ast::Function>),
    Block(NodeId<ast::Stmt>),
    Global,
}

#[derive(Clone, Debug)]
pub struct Variables {
    pub scopes: KeyedVec<ScopeId, Scope>,
    pub scope_children: Vec<ScopeId>,
    pub scope_symbols: Vec<SymbolId>,
    pub symbols: KeyedVec<SymbolId, Symbol>,
    pub ast_to_symbol: KeyedVec<NodeId<ast::Symbol>, SymbolId>,
}

impl Variables {
    pub fn new() -> Self {
        Variables {
            scopes: KeyedVec::new(),
            scope_children: Vec::new(),
            scope_symbols: Vec::new(),
            symbols: KeyedVec::new(),
            ast_to_symbol: KeyedVec::new(),
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
        let offset = scope.child_list_offset as usize;
        let len = scope.num_childeren as usize;

        &self.scope_children[offset..(offset + len)]
    }

    pub fn declared_vars(&self, id: ScopeId) -> &[SymbolId] {
        let scope = &self.scopes[id];
        let offset = scope.symbol_offset as usize;
        let len = scope.num_declarations as usize;

        &self.scope_symbols[offset..(offset + len)]
    }
}

impl Default for Variables {
    fn default() -> Self {
        Self::new()
    }
}

pub struct VariablesBuilder<'a> {
    ast: &'a mut Ast,
    variables: Variables,
    scope_stack: Vec<ScopeId>,
    symbol_stack: Vec<SymbolId>,
    current_scope: Option<ScopeId>,
}

impl<'a> VariablesBuilder<'a> {
    pub fn new(ast: &'a mut Ast) -> Self {
        Self {
            ast,
            variables: Variables::new(),
            scope_stack: Vec::new(),
            symbol_stack: Vec::new(),
            current_scope: None,
        }
    }
}

impl VariablesBuilder<'_> {
    pub fn build(self) -> Variables {
        assert_eq!(
            self.current_scope, None,
            "Tried to finish building variables while a scope was still on the stack"
        );
        self.variables
    }

    pub fn push_scope(&mut self, kind: ScopeKind) {
        if let Some(parent) = self.current_scope {
            self.variables.scopes[parent].num_childeren += 1;
            assert_ne!(kind, ScopeKind::Global);
        }

        let id = self.variables.scopes.push(Scope {
            parent: self.current_scope,
            kind,
            num_childeren: 0,
            child_list_offset: 0,
            num_declarations: 0,
            symbol_offset: 0,
        });

        self.scope_stack.push(id);
        self.current_scope = Some(id);
    }

    pub fn pop_scope(&mut self) -> Result<ScopeId> {
        let cur_scope = self
            .current_scope
            .expect("tried to pop a scope of an empty stack");
        let cur_scope_ref = &mut self.variables.scopes[cur_scope];
        self.current_scope = cur_scope_ref.parent;
        cur_scope_ref.child_list_offset = self
            .variables
            .scope_children
            .len()
            .try_into()
            .map_err(|_| Error::ExceededLimits(Limits::TooManyScopes))?;

        // Don't really care about 16 bit.
        let num_childeren: usize = cur_scope_ref.num_childeren.try_into().unwrap();
        let num_declarations: usize = cur_scope_ref.num_declarations.try_into().unwrap();
        // This conversion should always be valid since we can't have more than u32::MAX symbols.
        cur_scope_ref.symbol_offset = self.variables.scope_symbols.len() as u32;
        cur_scope_ref.child_list_offset = self.variables.scope_children.len() as u32;

        let len = self.scope_stack.len();
        self.variables
            .scope_children
            .extend_from_slice(&self.scope_stack[(len - num_childeren)..len]);
        self.scope_stack.truncate(len - num_childeren);

        let len = self.symbol_stack.len();
        self.variables
            .scope_symbols
            .extend_from_slice(&self.symbol_stack[(len - num_declarations)..len]);
        self.symbol_stack.truncate(len - num_declarations);

        Ok(cur_scope)
    }

    pub fn declare(
        &mut self,
        name: NodeId<ast::Symbol>,
        mut kind: Kind,
        at: Option<NodeId<ast::IdentOrPattern>>,
    ) -> Result<SymbolId> {
        let current_scope = self
            .current_scope
            .expect("tried to declare a variable without a scope");

        if kind == Kind::Function && self.variables.scopes[current_scope].kind == ScopeKind::Global
        {
            kind = Kind::Global
        }

        let ident = self.ast[name].name;

        let res = self.variables.symbols.push(Symbol {
            ident,
            kind,
            captured: false,
            declared: at,
            defined: None,
            last_use: None,
        });

        self.symbol_stack.push(res);
        self.variables.scopes[current_scope].num_declarations += 1;
        assert_eq!(self.variables.ast_to_symbol.next_id().id(), name.id());
        self.variables.ast_to_symbol.push(res);

        Ok(res)
    }

    fn resolve_variable_in(&self, name: StringId, mut scope: ScopeId) -> Option<SymbolId> {
        let mut variable_offset = self.symbol_stack.len();
        loop {
            let num_childeren = self.variables.scopes[scope].num_declarations as usize;
            variable_offset -= num_childeren;
            // reverse so we access symbols in order.
            for v in self.symbol_stack[variable_offset..(variable_offset + num_childeren)]
                .iter()
                .rev()
                .copied()
            {
                if self.variables.symbols[v].ident == name {
                    return Some(v);
                }
            }
            scope = self.variables.scopes[scope].parent?;
        }
    }

    fn add_unresolved(&mut self, name: NodeId<ast::Symbol>) -> SymbolId {
        let current_scope = self
            .current_scope
            .expect("tried to load a variable without a scope");
        let ident = self.ast[name].name;

        let res = self.variables.symbols.push(Symbol {
            ident,
            kind: Kind::Unresolved,
            captured: false,
            declared: None,
            defined: None,
            last_use: None,
        });

        self.symbol_stack.push(res);
        self.variables.scopes[current_scope].num_declarations += 1;
        assert_eq!(self.variables.ast_to_symbol.next_id(), name);
        self.variables.ast_to_symbol.push(res);
        res
    }

    pub fn load(&mut self, name: NodeId<ast::Symbol>, at: NodeId<ast::Expr>) -> Result<()> {
        let current_scope = self
            .current_scope
            .expect("tried to load a variable without a scope");
        if let Some(symbol) = self.resolve_variable_in(self.ast[name].name, current_scope) {
            assert_eq!(self.variables.ast_to_symbol.next_id().id(), name.id());
            self.variables.ast_to_symbol.push(symbol);

            let symbol = &mut self.variables.symbols[symbol];
            symbol.last_use = Some(at);
        } else {
            let symbol = self.add_unresolved(name);
            let symbol = &mut self.variables.symbols[symbol];
            symbol.last_use = Some(at);
        }
        Ok(())
    }

    pub fn store(&mut self, name: NodeId<ast::Symbol>, from: NodeId<ast::Expr>) -> Result<()> {
        let current_scope = self
            .current_scope
            .expect("tried to store a variable without a scope");
        if let Some(symbol) = self.resolve_variable_in(self.ast[name].name, current_scope) {
            assert_eq!(self.variables.ast_to_symbol.next_id().id(), name.id());
            self.variables.ast_to_symbol.push(symbol);

            self.store_symbol(symbol, from);
        } else {
            let symbol = self.add_unresolved(name);
            self.store_symbol(symbol, from);
        }
        Ok(())
    }

    pub fn store_symbol(&mut self, symbol: SymbolId, from: NodeId<ast::Expr>) {
        let symbol = &mut self.variables.symbols[symbol];
        if symbol.defined.is_none() {
            symbol.defined = Some(from);
        }
    }

    pub fn resolve_variables(&mut self, root: ListHead<ast::Stmt>) -> Result<()> {
        let ListHead::Present(mut head) = root else {
            return Ok(());
        };

        loop {
            self.resolve_stmt(self.ast[head].item)?;
            let Some(next) = self.ast[head].next else {
                break;
            };
            head = next;
        }
        Ok(())
    }
}
