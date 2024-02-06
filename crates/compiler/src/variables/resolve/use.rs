use ast::Ast;
use common::{hashmap::hash_map::HashMap, string::StringId};

use super::driver::VariableVisitor;
use crate::{
    variables::{
        Kind, LastUse, LoopId, LoopInfo, ScopeId, ScopeKind, Symbol, SymbolId, SymbolUseOrder,
        UseInfo, Variables,
    },
    Error, Limits, Result,
};

/// Pass which resolves all declared variables in the ast and creates the scopes.
pub struct UsePass<'a, 'b> {
    ast: &'a Ast,
    vars: &'b mut Variables,
    lookup: HashMap<StringId, SymbolId>,
    root: ScopeId,
    current: ScopeId,
    current_function: ScopeId,
    last: ScopeId,
    current_use: SymbolUseOrder,
    current_loop: Option<LoopId>,
}

impl<'a, 'b> UsePass<'a, 'b> {
    pub fn new(ast: &'a Ast, vars: &'b mut Variables, root_scope: ScopeId) -> Self {
        let mut lookup = HashMap::new();
        for v in vars.declared_vars(root_scope) {
            let k = vars.symbols[*v].ident;
            lookup.insert(k, *v);
        }

        Self {
            last: root_scope,
            current: root_scope,
            current_function: vars.function_of(root_scope),
            root: root_scope,
            lookup,
            ast,
            vars,
            current_use: SymbolUseOrder::first(),
            current_loop: None,
        }
    }

    fn advance_use(&mut self) -> Result<SymbolUseOrder> {
        let id = self.current_use;
        self.current_use = self
            .current_use
            .checked_add(1)
            .ok_or(Error::ExceededLimits(Limits::TooManyVariables))?;
        Ok(id)
    }
}

impl VariableVisitor for UsePass<'_, '_> {
    fn ast(&self) -> &Ast {
        self.ast
    }

    fn declare(&mut self, ast_node: ast::NodeId<ast::Symbol>, kind: Kind) -> Result<()> {
        let symbol = self.vars.ast_to_symbol[ast_node].id.unwrap();
        let id = self.advance_use()?;
        self.vars.symbols[symbol].declared = Some(id);
        Ok(())
    }

    fn use_symbol(&mut self, ast_node: ast::NodeId<ast::Symbol>) -> Result<()> {
        let ident = self.ast[ast_node].name;
        let id = if let Some(s) = self.lookup.get(&ident).copied() {
            if self.vars.symbols[s].kind != Kind::Unresolved {
                let symbol_scope = self.vars.symbols[s].scope;
                // check if the variable is used across a function.
                if symbol_scope < self.current_function {
                    self.vars.symbols[s].captured = true;
                }
                // assign a use to the variable.
                if let Some(current_loop) = self.current_loop {
                    if self.vars.loop_use[current_loop].scope > symbol_scope {
                        if !matches!(self.vars.symbols[s].last_use, LastUse::Loop(_)) {
                            self.vars.symbols[s].last_use = LastUse::Loop(current_loop);
                        }
                    } else {
                        self.vars.symbols[s].last_use = LastUse::Direct(self.advance_use()?);
                    }
                } else {
                    self.vars.symbols[s].last_use = LastUse::Direct(self.advance_use()?);
                }
            }
            s
        } else {
            let defined = self.advance_use()?;
            let symbol_id = self.vars.symbols.push(Symbol {
                ident,
                captured: false,
                kind: Kind::Unresolved,
                declared: Some(SymbolUseOrder::first()),
                defined: Some(defined),
                last_use: LastUse::Direct(SymbolUseOrder::last()),
                scope: self.root,
                shadows: None,
                ast_node,
            });

            self.lookup.insert(ident, symbol_id);

            self.vars.scopes[self.root].num_decl_children += 1;
            self.vars.scope_decls.push(symbol_id);
            symbol_id
        };

        self.vars.ast_to_symbol.insert_grow_default(
            ast_node,
            UseInfo {
                use_order: SymbolUseOrder::first(),
                id: Some(id),
            },
        );
        Ok(())
    }

    fn push_scope(&mut self, kind: ScopeKind) -> Result<()> {
        self.last = self.last.next();
        self.current = self.last;

        if self.vars.scopes[self.current].kind.is_function_scope() {
            self.current_function = self.current;
        }

        let offset = self.vars.scopes[self.current].decl_child_offset as usize;
        let len = self.vars.scopes[self.current].num_decl_children as usize;
        for v in self.vars.scope_decls[offset..(offset + len)]
            .iter()
            .copied()
        {
            let k = self.vars.symbols[v].ident;
            if let Some(old) = self.lookup.insert(k, v) {
                if self.vars.symbols[old].kind == Kind::Unresolved {
                    debug_assert_eq!(self.vars.symbols[v].shadows, None);
                }
                self.vars.symbols[v].shadows = Some(old);
            }
        }

        if let ScopeKind::Block { has_loop: true } = kind {
            self.current_loop = Some(self.vars.loop_use.push(LoopInfo {
                parent: self.current_loop,
                use_order: SymbolUseOrder::first(),
                scope: self.current,
            }));
        }

        debug_assert_eq!(self.vars.scopes[self.current].kind, kind);
        Ok(())
    }

    fn pop_scope(&mut self) -> Result<()> {
        for v in self.vars.declared_vars(self.current).iter().copied() {
            let k = self.vars.symbols[v].ident;

            if let Some(shadow) = self.vars.symbols[v].shadows {
                let res = self.lookup.insert(k, shadow);
                debug_assert!(res.is_some());
            } else {
                let res = self.lookup.remove(&k);
                debug_assert!(res.is_some());
            }
        }

        if let Some(current_loop) = self.current_loop {
            if self.current == self.vars.loop_use[current_loop].scope {
                self.vars.loop_use[current_loop].use_order = self.advance_use()?;
                self.current_loop = self.vars.loop_use[current_loop].parent;
            }
        }

        let current = self.current;
        self.current = self.vars.scopes[current]
            .parent
            .expect("tried to pop root scope");
        if self.vars.scopes[current].kind.is_function_scope() {
            self.current_function = self.vars.function_of(self.current);
        }
        Ok(())
    }
}
