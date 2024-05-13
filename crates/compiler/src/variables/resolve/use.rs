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

/// Pass which resolves all variable usages to a declared symbol and assigns use order to each
/// symbol use for use in compilation.
pub struct UsePass<'a, 'b> {
    ast: &'a Ast,
    vars: &'b mut Variables,
    lookup: HashMap<StringId, SymbolId>,
    root: ScopeId,
    current: ScopeId,
    current_function: ScopeId,
    last: ScopeId,
    current_use: SymbolUseOrder,
    loop_use: Option<SymbolUseOrder>,
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
            loop_use: None,
            current_loop: None,
        }
    }

    fn advance_use(&mut self) -> Result<SymbolUseOrder> {
        let id = self.current_use;
        self.current_use = self
            .current_use
            .checked_add(1)
            .ok_or(Error::Limit(Limits::TooManyVariables))?;
        self.loop_use = None;
        Ok(id)
    }

    fn advance_use_loop(&mut self) -> Result<SymbolUseOrder> {
        if let Some(x) = self.loop_use {
            return Ok(x);
        }
        let loop_use = self.advance_use()?;
        self.loop_use = Some(loop_use);
        Ok(loop_use)
    }
}

impl VariableVisitor for UsePass<'_, '_> {
    fn ast(&self) -> &Ast {
        self.ast
    }

    fn declare(&mut self, ast_node: ast::NodeId<ast::Symbol>, kind: Kind) -> Result<()> {
        let symbol = self.vars.ast_to_symbol[ast_node].id.unwrap();
        self.vars.symbols[symbol].declared = Some(self.current_use);
        Ok(())
    }

    fn use_symbol(&mut self, ast_node: ast::NodeId<ast::Symbol>) -> Result<()> {
        let ident = self.ast[ast_node].name;
        if let Some(s) = self.lookup.get(&ident).copied() {
            let use_order = self.advance_use()?;
            self.vars.ast_to_symbol.insert_grow_default(
                ast_node,
                UseInfo {
                    id: Some(s),
                    use_order,
                },
            );

            if self.vars.symbols[s].kind != Kind::Unresolved {
                let symbol_scope = self.vars.symbols[s].scope;
                // check if the variable is used across a function.
                if symbol_scope < self.current_function {
                    self.vars.symbols[s].captured = true;
                }

                self.vars.symbols[s].last_use = LastUse::Direct(use_order);
                if self.vars.symbols[s].defined.is_none() {
                    self.vars.symbols[s].defined = Some(use_order);
                }

                // assign a use to the variable.
                'calc_loop_use: {
                    let Some(current_loop) = self.current_loop else {
                        break 'calc_loop_use;
                    };
                    // we are in a loop
                    if let LastUse::Loop(x) = self.vars.symbols[s].last_use {
                        if self.vars.loop_use[x].use_order.is_none() {
                            // variable was assigned a loop order of a loop we haven't finished
                            // resolving so assignment is stil correct.
                            break 'calc_loop_use;
                        }
                    }
                    if self.vars.loop_use[current_loop].scope <= symbol_scope {
                        // symbol was declared within the current loop scope, so no need to change
                        break 'calc_loop_use;
                    }
                    // symbol crossed looping scope so we need to assign a loop use
                    // find the up most looping scope which is child of the symbols
                    // declaration scope and assign that as the symbols last use.
                    let mut root_loop = current_loop;
                    while let Some(p) = self.vars.loop_use[root_loop].parent {
                        if self.vars.loop_use[p].scope <= symbol_scope {
                            break;
                        }
                        root_loop = p;
                    }
                    self.vars.symbols[s].last_use = LastUse::Loop(root_loop)
                }
            }
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

            let use_order = self.advance_use()?;

            self.vars.ast_to_symbol.insert_grow_default(
                ast_node,
                UseInfo {
                    use_order,
                    id: Some(symbol_id),
                },
            );
        }

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
                use_order: None,
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
                self.vars.loop_use[current_loop].use_order = Some(self.advance_use_loop()?);
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
