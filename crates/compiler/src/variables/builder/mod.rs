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

use super::{Kind, Scope, ScopeId, ScopeKind, Symbol, SymbolId, Variables};

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

#[derive(Clone, Copy)]
struct ScopeStackValue {
    id: ScopeId,
    previous: Option<ScopeStackId>,
    uses: u32,
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
    scope_stack: KeyedVec<ScopeStackId, ScopeStackValue>,
    function_symbol_stack: KeyedVec<FunctionSymbolStackId, SymbolStackValue>,
    block_symbol_stack: KeyedVec<BlockSymbolStackId, SymbolStackValue>,
    use_stack: Vec<SymbolUse>,
    current_scope: Option<ScopeStackId>,
    /// Maps a name to an index into the symbol stack.
    lookup: HashMap<StringId, LookupValue>,
}

impl<'a> VariablesBuilder<'a> {
    pub fn new(ast: &'a mut Ast) -> Self {
        Self {
            ast,
            variables: Variables::new(),
            scope_stack: KeyedVec::new(),
            block_symbol_stack: KeyedVec::new(),
            function_symbol_stack: KeyedVec::new(),
            use_stack: Vec::new(),
            current_scope: None,
            lookup: HashMap::default(),
        }
    }
}

impl VariablesBuilder<'_> {
    pub fn build(self) -> Variables {
        if cfg!(debug_assertions) {
            let mut missing_variables = false;
            for (idx, v) in self.variables.ast_to_symbol.iter().enumerate() {
                if v.is_none() {
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
        debug_assert_eq!(
            self.variables
                .ast_to_symbol
                .iter()
                .position(|x| x.is_none()),
            None,
            "Missed a symbol during variable resolving"
        );
        assert_eq!(
            self.current_scope, None,
            "Tried to finish building variables while a scope was still on the stack"
        );
        self.variables
    }

    pub fn push_scope(&mut self, kind: ScopeKind) -> Result<ScopeId> {
        let parent = self.current_scope.map(|x| self.scope_stack[x].id);

        if let Some(parent) = parent {
            self.variables.scopes[parent].num_childeren += 1;
            debug_assert!(!matches!(kind, ScopeKind::Global { .. }));
        }

        let id = self
            .variables
            .scopes
            .try_push(Scope {
                parent,
                kind,
                num_childeren: 0,
                child_offset: 0,
                num_declarations: 0,
                symbol_offset: 0,
            })
            .map_err(|_| Error::ExceededLimits(Limits::TooManyScopes))?;

        let current = self.scope_stack.push(ScopeStackValue {
            id,
            previous: self.current_scope,
            uses: 0,
        });
        self.current_scope = Some(current);
        Ok(id)
    }

    pub fn pop_scope(&mut self) -> Result<ScopeId> {
        let current_scope_idx = self
            .current_scope
            .expect("tried to pop scope which doesn't exists.");
        let current_scope_id = self.scope_stack[current_scope_idx].id;
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

        for s in self.scope_stack.drain(offset..) {
            self.variables.scope_children.push(s.id);
        }

        // Update symbol offset
        current_scope.symbol_offset = self
            .variables
            .scope_symbols
            .len()
            .try_into()
            .map_err(|_| Error::ExceededLimits(Limits::TooManyVariables))?;

        // Resolve all uses and possibly add new symbols.
        let offset = self.use_stack.len() - self.scope_stack[current_scope_idx].uses as usize;
        let num_decls = current_scope.num_declarations;
        for s in self.use_stack.drain(offset..) {
            let name = self.ast[s.symbol].name;

            // lookup or create symbols.
            let id = match self.lookup.entry(name) {
                Entry::Occupied(x) => match *x.get() {
                    LookupValue::Block(x) => self.block_symbol_stack[x].id,
                    LookupValue::Function(x) => self.function_symbol_stack[x].id,
                    LookupValue::Unresolved(x) => x,
                },
                Entry::Vacant(x) => {
                    // variable could not be resolved.
                    // insert new unresolved variable.
                    // New unresolved symbols are added after the current symbol offset and before
                    // any other symbols.
                    self.variables.scopes[current_scope_id].num_declarations += 1;
                    let id = self.variables.symbols.push(Symbol {
                        ident: name,
                        captured: false,
                        kind: Kind::Unresolved,
                        declared: None,
                        defined: None,
                        last_use: None,
                        scope: current_scope_id,
                    });
                    self.variables.scope_symbols.push(id);
                    x.insert(LookupValue::Unresolved(id));
                    id
                }
            };

            // Update ast-symbol map.
            self.variables
                .ast_to_symbol
                .insert_grow(s.symbol, Some(id), None);

            let symbol = &mut self.variables.symbols[id];

            // Is the variable referenced across a function boundry.
            let mut cur = current_scope_id;
            while !symbol.captured && cur != symbol.scope {
                if self.variables.scopes[cur].kind.is_function_scope() {
                    symbol.captured = true;
                } else {
                    cur = self.variables.scopes[cur].parent.unwrap();
                }
            }

            if s.load {
                symbol.last_use = symbol.last_use.map(|x| x.max(s.at)).or(Some(s.at))
            } else {
                symbol.last_use = symbol.defined.map(|x| x.min(s.at)).or(Some(s.at))
            }
        }
        let current_scope = &mut self.variables.scopes[current_scope_id];

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
        self.current_scope = self.scope_stack[current_scope_idx].previous;
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

    pub fn declare(
        &mut self,
        name: NodeId<ast::Symbol>,
        mut kind: Kind,
        at: Option<NodeId<ast::IdentOrPattern>>,
    ) -> Result<SymbolId> {
        // The scope the variable is declared in.
        let current_scope_idx = self
            .current_scope
            .expect("tried to declare a variable without a scope");

        let mut current_scope = self.scope_stack[current_scope_idx].id;

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
                    // The hashmap should not contain any unresolved symbols except for a short duration
                    // when popping a scope.
                    LookupValue::Unresolved(x) => unreachable!(),
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
                        self.variables
                            .ast_to_symbol
                            .insert_grow(name, Some(collision), None);
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

        let res = self.variables.symbols.push(Symbol {
            ident,
            kind,
            captured: false,
            declared: at,
            defined: None,
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
        self.variables
            .ast_to_symbol
            .insert_grow(name, Some(res), None);

        Ok(res)
    }

    pub fn load(&mut self, symbol: NodeId<ast::Symbol>, at: NodeId<ast::Expr>) {
        let current_scope = self
            .current_scope
            .expect("tried to load a variable without a scope");

        self.scope_stack[current_scope].uses += 1;
        self.use_stack.push(SymbolUse {
            symbol,
            at,
            load: true,
        });
    }

    pub fn store(&mut self, symbol: NodeId<ast::Symbol>, at: NodeId<ast::Expr>) {
        let current_scope = self
            .current_scope
            .expect("tried to load a variable without a scope");

        self.scope_stack[current_scope].uses += 1;
        self.use_stack.push(SymbolUse {
            symbol,
            at,
            load: false,
        });
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
