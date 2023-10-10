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

use ast::{Ast, ListHead, NodeId};
use common::{
    hashmap::{hash_map::Entry, HashMap},
    id::KeyedVec,
    key,
    string::StringId,
};

use crate::{Error, Limits, Result};

use super::{
    Kind, Scope, ScopeId, ScopeKind, Symbol, SymbolId, SymbolUseOrder, UseInfo, Variables,
};

mod expr;
mod stmt;

key!(pub struct FunctionSymbolStackId(u32));
key!(pub struct BlockSymbolStackId(u32));
key!(pub struct ScopeStackId(u32));

#[derive(Clone, Copy)]
struct SymbolStackValue {
    previously_declared: Option<LookupValue>,
    id: SymbolId,
    ast: NodeId<ast::Symbol>,
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

pub struct VariablesBuilder<'a> {
    ast: &'a mut Ast,
    variables: Variables,
    scope_stack: Vec<ScopeId>,
    function_symbol_stack: KeyedVec<FunctionSymbolStackId, SymbolStackValue>,
    block_symbol_stack: KeyedVec<BlockSymbolStackId, SymbolStackValue>,
    current_scope: Option<ScopeId>,
    /// Maps a name to an index into the symbol stack.
    lookup: HashMap<StringId, LookupValue>,
    next_use_id: u32,
}

impl<'a> VariablesBuilder<'a> {
    pub fn new(ast: &'a mut Ast) -> Self {
        Self {
            ast,
            variables: Variables::new(),
            scope_stack: Vec::new(),
            block_symbol_stack: KeyedVec::new(),
            function_symbol_stack: KeyedVec::new(),
            current_scope: None,
            lookup: HashMap::default(),
            next_use_id: 0,
        }
    }
}

impl VariablesBuilder<'_> {
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
            self.variables.scope_symbols.push(s.id);
            let name = self.ast[s.ast].name;
            if let Some(prev) = s.previously_declared {
                self.lookup.insert(name, prev);
            } else {
                self.lookup.remove(&name);
            }
        }

        // set new current scope.
        self.current_scope = self.variables.scopes[current_scope_id].parent;
        Ok(current_scope_id)
    }

    fn redeclared(&self, symbol: NodeId<ast::Symbol>, previously: LookupValue) -> Error {
        let ast = match previously {
            LookupValue::Function(x) => self.function_symbol_stack[x].ast,
            LookupValue::Block(x) => self.block_symbol_stack[x].ast,
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

    fn map_use(&mut self, symbol_use: NodeId<ast::Symbol>, id: SymbolId) -> SymbolUseOrder {
        let use_order = SymbolUseOrder(self.next_use_id);
        let v = UseInfo {
            use_order,
            id: Some(id),
        };
        self.next_use_id += 1;
        self.variables.use_to_symbol.insert_grow(
            symbol_use,
            v,
            UseInfo {
                use_order: SymbolUseOrder(0),
                id: None,
            },
        );
        use_order
    }

    pub fn declare(
        &mut self,
        name: NodeId<ast::Symbol>,
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

        let ident = self.ast[name].name;

        let previously_declared = match self.lookup.entry(ident) {
            Entry::Occupied(mut x) => {
                let lookup = *x.get();
                let collision = match lookup {
                    LookupValue::Function(x) => self.function_symbol_stack[x].id,
                    LookupValue::Block(x) => self.block_symbol_stack[x].id,
                    LookupValue::Unresolved(symbol_id) => {
                        if kind.is_function_scoped()
                            || self.variables.scopes[current_scope]
                                .kind
                                .is_function_scope()
                        {
                            let id = LookupValue::Function(self.function_symbol_stack.next_id());
                            x.insert(id);
                        } else {
                            let id = LookupValue::Block(self.block_symbol_stack.next_id());
                            x.insert(id);
                        }

                        let use_order = self.map_use(name, symbol_id);
                        let symbol = &mut self.variables.symbols[symbol_id];
                        symbol.kind = kind;
                        symbol.declared = Some(use_order);
                        if initialized {
                            symbol.defined.get_or_insert(use_order);
                        }

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
                        return Err(self.redeclared(name, lookup));
                    } else {
                        let use_order = self.map_use(name, collision);
                        if initialized {
                            self.variables.symbols[collision]
                                .defined
                                .get_or_insert(use_order);
                        }
                        return Ok(collision);
                    }
                }

                if kind.is_function_scoped()
                    || self.variables.scopes[current_scope]
                        .kind
                        .is_function_scope()
                {
                    let id = LookupValue::Function(self.function_symbol_stack.next_id());
                    x.insert(id);
                } else {
                    let id = LookupValue::Block(self.block_symbol_stack.next_id());
                    x.insert(id);
                }
                Some(lookup)
            }
            Entry::Vacant(x) => {
                if kind.is_function_scoped()
                    || self.variables.scopes[current_scope]
                        .kind
                        .is_function_scope()
                {
                    let id = LookupValue::Function(self.function_symbol_stack.next_id());
                    x.insert(id);
                } else {
                    let id = LookupValue::Block(self.block_symbol_stack.next_id());
                    x.insert(id);
                }
                None
            }
        };

        let res = self.variables.symbols.next_id();
        let declared = self.map_use(name, res);
        self.variables.symbols.push(Symbol {
            ident,
            kind,
            captured: false,
            declared: Some(declared),
            defined: initialized.then_some(declared),
            last_use: None,
            scope: current_scope,
        });

        if kind.is_function_scoped()
            || self.variables.scopes[current_scope]
                .kind
                .is_function_scope()
        {
            self.function_symbol_stack.push(SymbolStackValue {
                previously_declared,
                id: res,
                ast: name,
            });
        } else {
            self.block_symbol_stack.push(SymbolStackValue {
                previously_declared,
                id: res,
                ast: name,
            });
        }
        self.variables.scopes[current_scope].num_declarations += 1;

        Ok(res)
    }

    fn unresolved_symbol(&mut self, symbol: NodeId<ast::Symbol>) -> SymbolId {
        let scope = self.current_scope.unwrap();
        let id = self.variables.symbols.push(Symbol {
            ident: self.ast[symbol].name,
            captured: false,
            kind: Kind::Unresolved,
            declared: None,
            defined: None,
            last_use: None,
            scope,
        });
        self.map_use(symbol, id);
        id
    }

    pub fn load(&mut self, use_symbol: NodeId<ast::Symbol>) {
        let current_scope = self
            .current_scope
            .expect("tried to load a variable without a scope");

        match self.lookup.entry(self.ast[use_symbol].name) {
            Entry::Occupied(x) => match x.get() {
                LookupValue::Block(x) => {
                    let id = self.block_symbol_stack[*x].id;
                    self.variables.symbols[id].last_use = Some(self.map_use(use_symbol, id));
                }
                LookupValue::Function(x) => {
                    let id = self.function_symbol_stack[*x].id;
                    self.variables.symbols[id].last_use = Some(self.map_use(use_symbol, id));
                }
                LookupValue::Unresolved(x) => {}
            },
            Entry::Vacant(x) => {
                let id = self.variables.symbols.next_id();
                x.insert(LookupValue::Unresolved(id));
                let first_use = self.map_use(use_symbol, id);
                self.variables.symbols.push(Symbol {
                    ident: self.ast[use_symbol].name,
                    captured: false,
                    kind: Kind::Unresolved,
                    declared: None,
                    defined: None,
                    last_use: Some(first_use),
                    scope: current_scope,
                });
            }
        }
    }

    pub fn store(&mut self, symbol: NodeId<ast::Symbol>, at: NodeId<ast::Expr>) {
        let current_scope = self
            .current_scope
            .expect("tried to load a variable without a scope");

        match self.lookup.entry(self.ast[symbol].name) {
            Entry::Occupied(x) => match *x.get() {
                LookupValue::Block(x) => {
                    let id = self.block_symbol_stack[x].id;
                    let use_order = self.map_use(symbol, id);
                    self.variables.symbols[id].defined.get_or_insert(use_order);
                }
                LookupValue::Function(x) => {
                    let id = self.function_symbol_stack[x].id;
                    let use_order = self.map_use(symbol, id);
                    self.variables.symbols[id].defined.get_or_insert(use_order);
                }
                LookupValue::Unresolved(x) => {
                    let use_order = self.map_use(symbol, x);
                    self.variables.symbols[x].defined.get_or_insert(use_order);
                }
            },
            Entry::Vacant(x) => {
                let id = self.variables.symbols.next_id();
                let defined = self.map_use(symbol, id);
                self.variables.symbols.push(Symbol {
                    ident: self.ast[symbol].name,
                    captured: false,
                    kind: Kind::Unresolved,
                    declared: None,
                    defined: Some(defined),
                    last_use: None,
                    scope: current_scope,
                });
            }
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
