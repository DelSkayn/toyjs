use ast::{visitor::Visitor, Ast, ListId};

use super::{ScopeId, Variables};
use crate::Result;

mod declare;
mod driver;
mod r#use;

use declare::DeclarePass;
use driver::VisitorDriver;
use r#use::UsePass;

pub fn resolve_script(
    root: ListId<ast::Stmt>,
    ast: &Ast,
    vars: &mut Variables,
    root_scope: ScopeId,
) -> Result<()> {
    let function_scope = vars.function_of(root_scope);

    let mut declare_pass = VisitorDriver::new(DeclarePass::new(ast, vars, root_scope));
    declare_pass.super_stmt_list(root)?;
    let mut inner = declare_pass.into_inner();
    // finish the global scope
    inner.finish_scope();

    let mut use_pass = VisitorDriver::new(UsePass::new(ast, vars, root_scope));
    use_pass.super_stmt_list(root)?;

    Ok(())
}
