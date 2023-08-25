use ast::{ListHead, NodeId};

use crate::{Compiler, Result};

impl<'a> Compiler<'a> {
    pub(super) fn resolve_stmts(&mut self, stmt: ListHead<ast::Stmt>) -> Result<()> {
        let ListHead::Present(mut head) = stmt else {
            return Ok(());
        };
        loop {
            self.resolve_stmt(self.ast[head].item)?;
            if let Some(next) = self.ast[head].next {
                head = next;
            } else {
                break;
            }
        }
        Ok(())
    }

    pub(super) fn resolve_stmt(&mut self, stmt: NodeId<ast::Stmt>) -> Result<()> {
        match self.ast[stmt] {
            ast::Stmt::Empty
            | ast::Stmt::Break { .. }
            | ast::Stmt::Continue { .. }
            | ast::Stmt::Debugger => {}
            ast::Stmt::Block { list } => {
                self.resolve_stmts(list)?;
            }
            ast::Stmt::VariableDecl { kind, decl } => to_do!(),
            ast::Stmt::Expr { expr } => {
                self.resolve_exprs(expr)?;
            }
            ast::Stmt::DoWhile { body, cond } => {
                self.resolve_stmt(body)?;
                self.resolve_exprs(cond)?;
            }
            ast::Stmt::If { cond, body, r#else } => {
                self.resolve_exprs(cond)?;
                self.resolve_stmt(body)?;
                if let Some(e) = r#else {
                    self.resolve_stmt(e)?;
                }
            }
            ast::Stmt::While { cond, body } => {
                self.resolve_exprs(cond)?;
                self.resolve_stmt(body)?;
            }
            ast::Stmt::For { head, body } => {
                //TODO
                self.resolve_stmt(body)?;
            }
            ast::Stmt::Switch {
                cond,
                cases,
                default,
            } => to_do!(),
            ast::Stmt::Throw { expr } => {
                self.resolve_exprs(expr)?;
            }
            ast::Stmt::Try {
                block,
                catch,
                finally,
            } => {
                self.resolve_stmts(block)?;

                if let Some(catch) = catch {
                    self.resolve_stmts(self.ast[catch].block)?;
                }

                if let Some(finally) = finally {
                    self.resolve_stmts(finally)?;
                }
            }
            ast::Stmt::With { expr, stmt } => to_do!(),
            ast::Stmt::Return { expr } => {
                if let Some(expr) = expr {
                    self.resolve_exprs(expr)?;
                }
            }
            ast::Stmt::Labeled { label, stmt } => {
                self.resolve_stmt(stmt)?;
            }
            ast::Stmt::Function { func } => self.resolve_func(func)?,
            ast::Stmt::Class { class } => self.resolve_class(class)?,
        }
        Ok(())
    }

    pub fn resolve_func(&mut self, func: NodeId<ast::Function>) -> Result<()> {
        to_do!()
    }

    pub fn resolve_class(&mut self, func: NodeId<ast::Class>) -> Result<()> {
        to_do!()
    }
}
