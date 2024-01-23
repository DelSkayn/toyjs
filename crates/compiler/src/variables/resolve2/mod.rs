use ast::{visitor::Visitor, Ast, ListId, NodeId};
use common::{id::KeyedVec, key};

use self::driver::{VariableVisitor, VisitorDriver};
use super::{Scope, ScopeId, ScopeKind, SymbolId, Variables};
use crate::{Error, Limits, Result};

mod declare;
mod driver;

pub fn resolve_script(
    root: ListId<ast::Stmt>,
    ast: &Ast,
    vars: &mut Variables,
    root_scope: ScopeId,
) -> Result<()> {
    let function_scope = vars.function_of(root_scope);

    let mut declare_pass = VisitorDriver::new(DeclarePass::new(ast, vars, root_scope));
    declare_pass.super_stmt_list(root)?;

    let mut use_pass = VisitorDriver::new(UsePass {
        next: root_scope,
        current: vars.function_of(root_scope),
        ast,
        vars,
    });
    use_pass.super_stmt_list(root)?;

    Ok(())
}

/// Pass which resolves all declared variables in the ast and creates the scopes.
pub struct UsePass<'a, 'b> {
    ast: &'a Ast,
    vars: &'b mut Variables,
    current: ScopeId,
    next: ScopeId,
}

impl VariableVisitor for UsePass<'_, '_> {
    fn ast(&self) -> &Ast {
        self.ast
    }
}
