//! The Variable builder has a somewhat complicated structure.
//! This was primarily done to avoid having to create a ton of nested hashmap where each hashmap
//! would refer to the next hashmap if symbol could not be resolved there.
//!
//! Instead the builder contains a single hashmap which maps strings to a symbol in the symbol
//! stack.
//!
//! The scope stack functions as following: It contains all the parent scopes as well the child scopes
//! which are currently resolved of the current scope. When the scope is popped only the child
//! scopes are removed from the stack and the previously current scope is left on the stack, now as
//! a child of the new current scope.
//!
//! The symbol stack consists of two arrays the function and the block symbol stack.
//! Both of these stacks are appended to as more scopes are traversed.
//! The symbol stack contains all varaibles currently declared of the current and all parent scopes.
//! It is seperated into two arrays because otherwise we couldn't build it as a stack: A
//! function scope may at any point get additional declarations from its child block scopes from
//! `var`-like declaretions. That's why all `var`-like declarations are put into the
//! function_symbol_stack and all other declarations are int the block_symbol_stack. This way both
//! can be build like a stack.
//!
//! When a variable is declared, it is added to the hashmap and pushed onto the appropriate stack.
//! Then when a scope is popped all the id's of the symbols are added to the scope_symbols array,
//! the scope offset is set to the start of the added symbols. This way all scopes can share the
//! same array for their childern. The symbols which are now no longer declared are removed then
//! removed from the hashmap

use super::{
    Kind, Scope, ScopeId, ScopeKind, Symbol, SymbolId, SymbolUseOrder, UseInfo, Variables,
};
use crate::{Error, Limits, Result};
use ast::{visitor::Visitor, Ast, IdentOrPattern, ListHead, NodeId};
use common::{
    hashmap::{hash_map::Entry, HashMap},
    id::KeyedVec,
    key,
    string::StringId,
};

key!(pub struct FunctionSymbolStackId(u32));
key!(pub struct BlockSymbolStackId(u32));
key!(pub struct ScopeStackId(u32));

#[derive(Clone, Copy)]
struct SymbolStackValue {
    previously_declared: Option<LookupValue>,
    symbol_id: SymbolId,
    ast_node: NodeId<ast::Symbol>,
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum LookupValue {
    Block(BlockSymbolStackId),
    Function(FunctionSymbolStackId),
    Unresolved(SymbolId),
}

pub struct SymbolUse {
    symbol: NodeId<ast::Symbol>,
    at: NodeId<ast::Expr>,
    load: bool,
}

pub struct VariablesResolver<'a> {
    ast: &'a mut Ast,
    variables: Variables,
    scope_stack: Vec<ScopeId>,
    function_symbol_stack: KeyedVec<FunctionSymbolStackId, SymbolStackValue>,
    block_symbol_stack: KeyedVec<BlockSymbolStackId, SymbolStackValue>,
    current_scope: Option<ScopeId>,
    last_function_scope: Option<ScopeId>,
    /// Maps a name to an index into the symbol stack.
    lookup: HashMap<StringId, LookupValue>,
    declaring: Option<Kind>,
}

impl<'a> VariablesResolver<'a> {
    pub fn new(ast: &'a mut Ast) -> Self {
        Self {
            ast,
            variables: Variables::new(),
            scope_stack: Vec::new(),
            block_symbol_stack: KeyedVec::new(),
            function_symbol_stack: KeyedVec::new(),
            current_scope: None,
            last_function_scope: None,
            lookup: HashMap::default(),
            declaring: None,
        }
    }
}

impl VariablesResolver<'_> {
    pub fn build(self) -> Variables {
        if cfg!(debug_assertions) {
            let mut missing_variables = false;
            for (idx, v) in self.variables.use_to_symbol.iter().enumerate() {
                if v.id.is_none() {
                    println!(
                        "missed symbol [{:?}]",
                        NodeId::<ast::Symbol>::try_from(idx).unwrap()
                    );
                    missing_variables = true;
                }
            }
            if missing_variables {
                panic!("missed symbols during symbol resolution step");
            }
        }
        assert_eq!(
            self.current_scope, None,
            "Tried to finish building variables while a scope was still on the stack"
        );
        self.variables
    }

    pub fn push_scope(&mut self, kind: ScopeKind) -> Result<ScopeId> {
        if let Some(parent) = self.current_scope {
            self.variables.scopes[parent].num_childeren += 1;
            debug_assert!(!matches!(kind, ScopeKind::Global { .. }));
        }

        let id = self
            .variables
            .scopes
            .try_push(Scope {
                parent: self.current_scope,
                kind,
                num_childeren: 0,
                child_offset: 0,
                num_declarations: 0,
                symbol_offset: 0,
            })
            .map_err(|_| Error::ExceededLimits(Limits::TooManyScopes))?;

        self.scope_stack.push(id);
        self.current_scope = Some(id);
        if kind.is_function_scope() {
            self.last_function_scope = Some(id);
        }
        Ok(id)
    }

    pub fn pop_scope(&mut self) -> Result<ScopeId> {
        let current_scope_id = self
            .current_scope
            .expect("tried to pop scope which doesn't exists.");
        let current_scope = &mut self.variables.scopes[current_scope_id];

        // Push all the children of the current scope onto the child list.
        current_scope.child_offset = self
            .variables
            .scope_children
            .len()
            .try_into()
            .map_err(|_| Error::ExceededLimits(Limits::TooManyScopes))?;
        let offset = self.scope_stack.len() - current_scope.num_childeren as usize;
        self.variables
            .scope_children
            .reserve(current_scope.num_childeren as usize);

        self.variables
            .scope_children
            .extend_from_slice(&self.scope_stack[offset..]);
        self.scope_stack.truncate(offset);

        // Update symbol offset
        current_scope.symbol_offset = self
            .variables
            .scope_symbols
            .len()
            .try_into()
            .map_err(|_| Error::ExceededLimits(Limits::TooManyVariables))?;

        let num_decls = current_scope.num_declarations;
        // remove temporarly added unresolved symbol names from the hashmap.
        for sym in &self.variables.scope_symbols[(current_scope.symbol_offset as usize)..] {
            self.lookup.remove(&self.variables.symbols[*sym].ident);
        }

        // Add declared symbols.
        let iter = if current_scope.kind.is_function_scope() {
            let offset: usize = self.function_symbol_stack.len() - num_decls as usize;
            self.function_symbol_stack.drain(offset..)
        } else {
            let offset: usize = self.block_symbol_stack.len() - num_decls as usize;
            self.block_symbol_stack.drain(offset..)
        };
        for s in iter {
            self.variables.scope_symbols.push(s.symbol_id);
            let name = self.ast[s.ast_node].name;
            if let Some(prev) = s.previously_declared {
                self.lookup.insert(name, prev);
            } else {
                self.lookup.remove(&name);
            }
        }

        // set new function scope.
        let current_scope = &self.variables.scopes[current_scope_id];
        if current_scope.kind.is_function_scope() {
            self.last_function_scope = current_scope.parent.map(|x| self.variables.function_of(x));
        }

        // set new current scope.
        self.current_scope = current_scope.parent;
        Ok(current_scope_id)
    }

    fn redeclared(&self, symbol: NodeId<ast::Symbol>, previously: LookupValue) -> Error {
        let ast = match previously {
            LookupValue::Function(x) => self.function_symbol_stack[x].ast_node,
            LookupValue::Block(x) => self.block_symbol_stack[x].ast_node,
            // The hashmap should not contain any unresolved symbols except for a short duration
            // when popping a scope.
            LookupValue::Unresolved(_) => unreachable!(),
        };
        let first_declared = self.ast[ast].span;
        Error::Redeclared {
            span: self.ast[symbol].span,
            first_declared,
        }
    }

    pub fn declare(
        &mut self,
        ast_node: NodeId<ast::Symbol>,
        mut kind: Kind,
        initialized: bool,
    ) -> Result<SymbolId> {
        // The scope the variable is declared in.
        let mut current_scope = self
            .current_scope
            .expect("tried to declare a variable without a scope");

        if kind == Kind::Function {
            // Update the scope to function like scope where the variable will be declared.
            current_scope = self.variables.function_of(current_scope);
            if matches!(
                self.variables.scopes[current_scope].kind,
                ScopeKind::Global { .. }
            ) {
                kind = Kind::Global
            }
        }

        let ident = self.ast[ast_node].name;

        match self.lookup.entry(ident) {
            Entry::Occupied(mut entry) => {
                let lookup = *entry.get();
                let collision = match lookup {
                    LookupValue::Function(x) => self.function_symbol_stack[x].symbol_id,
                    LookupValue::Block(x) => self.block_symbol_stack[x].symbol_id,
                    LookupValue::Unresolved(symbol_id) => {
                        let id = if kind.is_function_scoped()
                            || self.variables.scopes[current_scope]
                                .kind
                                .is_function_scope()
                        {
                            let id = self.function_symbol_stack.push(SymbolStackValue {
                                previously_declared: Some(lookup),
                                symbol_id,
                                ast_node,
                            });
                            LookupValue::Function(id)
                        } else {
                            let id = self.block_symbol_stack.push(SymbolStackValue {
                                previously_declared: Some(lookup),
                                symbol_id,
                                ast_node,
                            });
                            LookupValue::Block(id)
                        };
                        entry.insert(id);

                        let symbol = &mut self.variables.symbols[symbol_id];
                        symbol.kind = kind;
                        return Ok(symbol_id);
                    }
                };

                if self.variables.symbols[collision].scope == current_scope {
                    let collision_kind = self.variables.symbols[collision].kind;
                    if kind.is_block_scoped()
                        || collision_kind.is_block_scoped()
                        || kind.is_arg() && collision_kind.is_arg()
                    {
                        // redeclaration, dissallowed for let like declarations.
                        // Return an error.
                        return Err(self.redeclared(ast_node, lookup));
                    } else {
                        return Ok(collision);
                    }
                }

                let symbol_id = self.variables.symbols.next_id();
                let lookup = if kind.is_function_scoped()
                    || self.variables.scopes[current_scope]
                        .kind
                        .is_function_scope()
                {
                    let id = self.function_symbol_stack.push(SymbolStackValue {
                        previously_declared: Some(lookup),
                        symbol_id,
                        ast_node,
                    });
                    LookupValue::Function(id)
                } else {
                    let id = self.block_symbol_stack.push(SymbolStackValue {
                        previously_declared: Some(lookup),
                        symbol_id,
                        ast_node,
                    });
                    LookupValue::Block(id)
                };
                entry.insert(lookup);
            }
            Entry::Vacant(entry) => {
                let symbol_id = self.variables.symbols.next_id();
                let lookup = if kind.is_function_scoped()
                    || self.variables.scopes[current_scope]
                        .kind
                        .is_function_scope()
                {
                    let stack_id = self.function_symbol_stack.push(SymbolStackValue {
                        previously_declared: None,
                        symbol_id,
                        ast_node,
                    });
                    LookupValue::Function(stack_id)
                } else {
                    let stack_id = self.block_symbol_stack.push(SymbolStackValue {
                        previously_declared: None,
                        symbol_id,
                        ast_node,
                    });
                    LookupValue::Block(stack_id)
                };
                entry.insert(lookup);
            }
        };

        let symbol_id = self.variables.symbols.next_id();
        self.variables.symbols.push(Symbol {
            ident,
            kind,
            captured: false,
            scope: current_scope,
            last_use: None,
            defined: None,
            declared: None,
        });
        self.variables.scopes[current_scope].num_declarations += 1;
        Ok(symbol_id)
    }

    fn unresolved_symbol(&mut self, ast_node: NodeId<ast::Symbol>) -> SymbolId {
        let scope = self.current_scope.unwrap();
        self.variables.symbols.push(Symbol {
            ident: self.ast[ast_node].name,
            captured: false,
            kind: Kind::Unresolved,
            declared: None,
            defined: None,
            last_use: None,
            scope,
        })
    }

    pub fn load(&mut self, ast_node: NodeId<ast::Symbol>) {
        let current_scope = self
            .current_scope
            .expect("tried to load a variable without a scope");

        let id = match self.lookup.entry(self.ast[ast_node].name) {
            Entry::Occupied(entry) => match *entry.get() {
                LookupValue::Block(stack_id) => self.block_symbol_stack[stack_id].symbol_id,
                LookupValue::Function(stack_id) => self.function_symbol_stack[stack_id].symbol_id,
                LookupValue::Unresolved(symbol_id) => symbol_id,
            },
            Entry::Vacant(entry) => {
                let symbol_id = self.variables.symbols.next_id();
                entry.insert(LookupValue::Unresolved(symbol_id));
                self.variables.symbols.push(Symbol {
                    ident: self.ast[ast_node].name,
                    captured: false,
                    kind: Kind::Unresolved,
                    declared: None,
                    defined: None,
                    last_use: None,
                    scope: current_scope,
                });
                return;
            }
        };
        let symbol = &mut self.variables.symbols[id];
        // Used across a function boundery.
        if symbol.scope < self.last_function_scope.unwrap() {
            symbol.captured = true;
        }
    }

    pub fn store(&mut self, ast_node: NodeId<ast::Symbol>) {
        let current_scope = self
            .current_scope
            .expect("tried to load a variable without a scope");

        match self.lookup.entry(self.ast[ast_node].name) {
            Entry::Occupied(_) => {}
            Entry::Vacant(x) => {
                let id = self.variables.symbols.next_id();
                self.variables.symbols.push(Symbol {
                    ident: self.ast[ast_node].name,
                    captured: false,
                    kind: Kind::Unresolved,
                    declared: None,
                    defined: None,
                    last_use: None,
                    scope: current_scope,
                });
            }
        }
    }

    pub fn resolve_variables(&mut self, root: ListHead<ast::Stmt>) -> Result<()> {
        let ListHead::Present(head) = root else {
            return Ok(());
        };
        self.super_stmt_list(head)?;
        Ok(())
    }

    fn resolve_params(
        &mut self,
        params: ListHead<ast::BindingElement>,
        rest: Option<NodeId<IdentOrPattern>>,
    ) -> Result<()> {
        let before = self.declaring.replace(Kind::Arg);
        if let ListHead::Present(p) = params {
            self.super_binding_element_list(p)?;
        }
        if let Some(p) = rest {
            self.super_ident_or_pattern(p)?;
        }
        self.declaring = before;
        Ok(())
    }
}

impl Visitor<Error> for VariablesResolver<'_> {
    fn ast(&self) -> &Ast {
        self.ast
    }

    fn super_function(&mut self, func: NodeId<ast::Function>) -> Result<()> {
        match self.ast[func] {
            ast::Function::Arrow {
                params,
                rest_param,
                body,
                ..
            } => {
                self.push_scope(ScopeKind::Function(func))?;
                self.resolve_params(params, rest_param)?;
                match body {
                    ast::ArrowFunctionBody::Expr(x) => self.super_expr(x)?,
                    ast::ArrowFunctionBody::Stmt(x) => {
                        if let ListHead::Present(x) = x {
                            self.super_stmt_list(x)?;
                        }
                    }
                }
                self.pop_scope()?;
            }
            ast::Function::Declared {
                name,
                params,
                rest_param,
                body,
                ..
            } => {
                self.push_scope(ScopeKind::Function(func))?;
                self.resolve_params(params, rest_param)?;
                if let ListHead::Present(x) = body {
                    self.super_stmt_list(x)?;
                }
                self.pop_scope()?;
                self.declare(name, Kind::Function, true)?;
            }
            ast::Function::Expr {
                name,
                params,
                rest_param,
                body,
                ..
            } => {
                self.push_scope(ScopeKind::Function(func))?;
                self.resolve_params(params, rest_param)?;
                if let ListHead::Present(x) = body {
                    self.super_stmt_list(x)?;
                }
                self.pop_scope()?;
                if let Some(name) = name {
                    self.declare(name, Kind::Function, true)?;
                }
            }
        }

        Ok(())
    }

    fn super_stmt(&mut self, stmt: NodeId<ast::Stmt>) -> std::prelude::v1::Result<(), Error> {
        match self.ast[stmt] {
            ast::Stmt::Block { .. } => {
                self.push_scope(ScopeKind::Block)?;
                self.visit_stmt(stmt)?;
                self.pop_scope()?;
                return Ok(());
            }
            ast::Stmt::VariableDecl { kind, decl } => {
                let before = self.declaring.replace(kind.into());
                self.super_variable_decl_list(decl)?;
                self.declaring = before;
            }
            _ => (),
        }
        self.visit_stmt(stmt)
    }

    fn super_head_pre(
        &mut self,
        head: NodeId<ast::ForLoopHead>,
    ) -> std::prelude::v1::Result<(), Error> {
        match self.ast[head] {
            ast::ForLoopHead::In { decl, .. } | ast::ForLoopHead::Of { decl, .. } => {
                if let ast::InOfDecl::Decl { kind, binding } = decl {
                    let before = self.declaring.replace(kind.into());
                    self.super_ident_or_pattern(binding)?;
                    self.declaring = before;
                }
            }

            _ => {}
        }
        self.visit_head_pre(head)
    }

    fn super_class_member(
        &mut self,
        cls_mem: NodeId<ast::ClassMember>,
    ) -> std::prelude::v1::Result<(), Error> {
        if let ast::ClassMember::StaticBlock { .. } = self.ast[cls_mem] {
            self.push_scope(ScopeKind::Static)?;
            self.visit_class_member(cls_mem)?;
            self.pop_scope()?;
            return Ok(());
        }
        self.visit_class_member(cls_mem)
    }

    fn super_expr(&mut self, expr: NodeId<ast::Expr>) -> Result<()> {
        let before = self.declaring.take();
        self.visit_expr(expr)?;
        self.declaring = before;
        Ok(())
    }
}
