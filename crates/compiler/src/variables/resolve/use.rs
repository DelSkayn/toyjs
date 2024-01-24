use ast::Ast;
use common::{hashmap::hash_map::HashMap, string::StringId};

use super::driver::VariableVisitor;
use crate::variables::{
    Kind, ScopeId, ScopeKind, Symbol, SymbolId, SymbolUseOrder, UseInfo, Variables,
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
        }
    }
}

impl VariableVisitor for UsePass<'_, '_> {
    fn ast(&self) -> &Ast {
        self.ast
    }

    fn use_symbol(&mut self, ast_node: ast::NodeId<ast::Symbol>) -> crate::Result<()> {
        let ident = self.ast[ast_node].name;
        let id = if let Some(s) = self.lookup.get(&ident).copied() {
            if self.vars.symbols[s].scope < self.current_function {
                self.vars.symbols[s].captured = true;
            }
            s
        } else {
            let symbol_id = self.vars.symbols.push(Symbol {
                ident,
                captured: false,
                kind: Kind::Unresolved,
                declared: None,
                defined: None,
                last_use: None,
                scope: self.root,
                shadows: None,
                ast_node,
            });

            self.lookup.insert(ident, symbol_id);

            self.vars.scopes[self.root].num_decl_children += 1;
            self.vars.scope_decls.push(symbol_id);
            symbol_id
        };

        self.vars.use_to_symbol.insert_grow_default(
            ast_node,
            UseInfo {
                use_order: SymbolUseOrder::first(),
                id: Some(id),
            },
        );
        Ok(())
    }

    fn push_scope(&mut self, kind: ScopeKind) -> crate::Result<()> {
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
                    debug_assert!(self.vars.symbols[v].shadows.is_none());
                    self.vars.symbols[v].shadows = Some(old);
                }
            }
        }
        debug_assert_eq!(self.vars.scopes[self.current].kind, kind);
        Ok(())
    }

    fn pop_scope(&mut self) -> crate::Result<()> {
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
