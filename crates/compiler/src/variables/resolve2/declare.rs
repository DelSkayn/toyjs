use ast::{Ast, NodeId};
use common::{
    hashmap::{
        hash_map::{Entry, HashMap},
        HashMap,
    },
    id::KeyedVec,
    key,
    string::StringId,
};

use super::driver::VariableVisitor;
use crate::{
    variables::{Kind, Scope, ScopeId, ScopeKind, Symbol, SymbolId, Variables},
    Error, Limits, Result,
};

key!(pub struct FunctionSymbolStackId(u32));
key!(pub struct BlockSymbolStackId(u32));

pub enum SymbolStackPtr {
    Block(BlockSymbolStackId),
    Function(FunctionSymbolStackId),
}

/// Pass which resolves all declared variables in the ast and creates the scopes.
pub struct DeclarePass<'a, 'b> {
    ast: &'a Ast,
    vars: &'b mut Variables,
    function_decl_stack: Vec<SymbolId>,
    block_decl_stack: Vec<SymbolId>,
    scope_stack: Vec<ScopeId>,
    lookup: HashMap<StringId, SymbolId>,
    current_block: ScopeId,
    current_function: ScopeId,
}

impl<'a, 'b> DeclarePass<'a, 'b> {
    pub fn new(ast: &'a Ast, vars: &'b mut Variables, root: ScopeId) -> Self {
        let current_function = vars.function_of(root);
        DeclarePass {
            ast,
            vars,
            function_decl_stack: Vec::new(),
            block_decl_stack: Vec::new(),
            scope_stack: Vec::new(),
            lookup: HashMap::new(),
            current_block: root,
            current_function,
        }
    }
}

impl VariableVisitor for DeclarePass<'_, '_> {
    fn ast(&self) -> &Ast {
        self.ast
    }

    fn push_scope(&mut self, kind: ScopeKind) -> Result<()> {
        let id = self
            .vars
            .scopes
            .try_push(Scope {
                parent: Some(self.current_block),
                kind,
                num_scope_children: 0,
                scope_child_offset: 0,
                num_decl_children: 0,
                decl_child_offset: 0,
            })
            .map_err(|_| Error::ExceededLimits(Limits::TooManyScopes))?;
        self.vars.scopes[self.current_block].num_scope_children += 1;
        self.scope_stack.push(id);

        self.current_block = id;
        if kind.is_function_scope() {
            self.current_function = id
        }

        Ok(())
    }

    fn pop_scope(&mut self) -> Result<()> {
        let scope = &mut self.vars.scopes[self.current_block];
        let is_function_scope = scope.kind.is_function_scope();
        let num_childeren = scope.num_scope_children;

        // push declared scopes into the stack.
        let offset = self.scope_stack.len() - num_childeren as usize;
        let scope_childeren_offset = self.vars.scope_children.len();
        self.vars
            .scope_children
            .extend(self.scope_stack.drain(offset..));
        scope.scope_child_offset = offset as u32;

        // update current scope.
        self.current_block = scope.parent.expect("tried to pop the global scope");
        if is_function_scope {
            self.current_function = self.vars.function_of(self.current_block);
        }
        Ok(())
    }

    fn declare(&mut self, ast_node: NodeId<ast::Symbol>, kind: Kind) -> Result<()> {
        let kind = if kind.is_function_scoped()
            && matches!(
                self.vars.scopes[self.current_function].kind,
                ScopeKind::Global { .. },
            ) {
            Kind::Global
        } else {
            kind
        };

        let ident = self.ast[ast_node].name;
        let symbol_id = match self.lookup.entry(ident) {
            Entry::Occupied(mut entry) => {
                let old_symbol_id = *entry.get();
                if self.vars.symbols[old_symbol_id].scope == self.current_block {
                    let collision_kind = self.vars.symbols[old_symbol_id].kind;
                    if kind.is_block_scoped()
                        || collision_kind.is_block_scoped()
                        || kind.is_arg() && collision_kind.is_arg()
                    {
                        let old_ast = self.vars.symbols[old_symbol_id].ast_node;
                        return Err(Error::Redeclared {
                            span: self.ast[ast_node].span,
                            first_declared: self.ast[old_ast].span,
                        });
                    }

                    // redeclaration, no new symbol needed.
                    return Ok(());
                }

                // we did not collide at this point.
                let scope = if kind.is_function_scoped() {
                    self.current_function
                } else {
                    self.current_block
                };

                let symbol_id = self.vars.symbols.push(Symbol {
                    ident,
                    captured: false,
                    kind,
                    declared: None,
                    defined: None,
                    last_use: None,
                    scope,
                    shadows: Some(old_symbol_id),
                    ast_node,
                });

                entry.insert(symbol_id);

                symbol_id
            }
            Entry::Vacant(mut entry) => {
                let scope = if kind.is_function_scoped() {
                    self.current_function
                } else {
                    self.current_block
                };

                let symbol_id = self.vars.symbols.push(Symbol {
                    ident,
                    captured: false,
                    kind,
                    declared: None,
                    defined: None,
                    last_use: None,
                    scope,
                    shadows: None,
                    ast_node,
                });

                entry.insert(symbol_id);

                symbol_id
            }
        };

        if kind.is_function_scoped() || self.current_function == self.current_block {
            self.function_decl_stack.push(symbol_id);
            self.vars.scopes[self.current_function].num_decl_children += 1;
        } else {
            self.block_decl_stack.push(symbol_id);
            self.vars.scopes[self.current_block].num_decl_children += 1;
        }

        Ok(())
    }
}
