use ast::{ListHead, NodeId};
use common::string::StringId;

use super::{Compiler, Result};

mod expr;
mod stmt;

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub enum Kind {
    /// Defined with the side effect of introducing new global variables.
    Global,
    /// Define within the function scope.
    Function,
    /// Defined with let
    Let,
    /// Defined with const
    Const,
    /// Variable is used without ever being declared.
    Unresolved,
}

pub struct Lifetime {
    /// When the variable is first declared.
    declared: NodeId<ast::IdentOrPattern>,
    /// When the variable is defined.
    defined: Option<NodeId<ast::Expr>>,
    /// When the variable is last used.
    last_use: Option<NodeId<ast::Expr>>,
}

pub struct VariableInfo {
    /// The identifier of the variable.
    ident: StringId,
    /// Is the variable captured by a closure.
    captured: bool,
    /// How is the variable declared.
    kind: Kind,
    /// How long does the variable life.
    lifetime: Option<Lifetime>,
}

pub struct Variables {}

impl Variables {
    pub fn new() -> Self {
        Variables {}
    }

    pub fn load(&mut self, name: StringId, at: NodeId<ast::Expr>) -> Result<()> {
        to_do!()
    }

    pub fn store(&mut self, name: StringId, from: NodeId<ast::Expr>) -> Result<()> {
        to_do!()
    }

    pub fn declare(
        &mut self,
        name: StringId,
        kind: Kind,
        at: NodeId<ast::IdentOrPattern>,
    ) -> Result<()> {
        to_do!()
    }
}

impl<'a> Compiler<'a> {
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

    pub fn use_variable(name: StringId, expr: NodeId<ast::Expr>) -> Result<()> {
        Ok(())
    }
}
