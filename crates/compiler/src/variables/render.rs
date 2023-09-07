use core::fmt;

use ast::Ast;
use common::structs::Interners;

use super::{ScopeId, Variables};

pub struct RenderVariables<'a> {
    root: ScopeId,
    variables: &'a Variables,
    ast: &'a Ast,
    interners: &'a Interners,
}

impl RenderVariables<'_> {
    fn fmt_scope(&self, scope: ScopeId, indent: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.variables.scopes()[scope].kind {
            super::ScopeKind::Function(_) => writeln!(f, "{:indent$}> FUNCTION", "")?,
            super::ScopeKind::Block(_) => writeln!(f, "{:indent$}> BLOCK", "")?,
            super::ScopeKind::Global => writeln!(f, "{:indent$}> GLOBAL", "")?,
        }
        for s in self.variables.declared_vars(scope).iter().copied() {
            let ident = self.variables.symbols()[s].ident;
            writeln!(
                f,
                "{:indent$}- {} KIND: {:?} FROM {} TO {}",
                "",
                self.interners.strings.get(ident).unwrap(),
                self.variables.symbols()[s].kind,
                self.variables.symbols()[s]
                    .defined
                    .map(|x| x.id() as isize)
                    .unwrap_or(-1),
                self.variables.symbols()[s]
                    .last_use
                    .map(|x| x.id() as isize)
                    .unwrap_or(-1)
            )?;
        }
        for s in self.variables.child_scopes(scope).iter().copied() {
            self.fmt_scope(s, indent + 2, f)?;
        }
        Ok(())
    }
}

impl fmt::Display for RenderVariables<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_scope(self.root, 0, f)
    }
}

impl Variables {
    pub fn render<'a>(
        &'a self,
        root: ScopeId,
        ast: &'a Ast,
        interners: &'a Interners,
    ) -> RenderVariables {
        RenderVariables {
            root,
            variables: self,
            ast,
            interners,
        }
    }
}
