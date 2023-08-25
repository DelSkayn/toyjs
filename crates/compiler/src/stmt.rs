use ast::{ListHead, NodeId};

use crate::{Compiler, Result};

impl<'a> Compiler<'a> {
    pub fn compile_stmt(&mut self, stmt: NodeId<ast::Stmt>) -> Result<Option<i8>> {
        match self.ast[stmt] {
            ast::Stmt::Block { list } => {
                let ListHead::Present(mut head) = list else {
                    return Ok(None);
                };
                loop {
                    let res = self.compile_stmt(self.ast[head].item)?;
                    if let Some(x) = self.ast[head].next {
                        head = x;
                    } else {
                        return Ok(res);
                    }
                }
            }
            ast::Stmt::VariableDecl { kind, decl } => to_do!(),
            ast::Stmt::Empty => Ok(None),
            ast::Stmt::Expr { expr } => self.compile_exprs(expr).map(Some),
            ast::Stmt::DoWhile { body, cond } => to_do!(),
            ast::Stmt::If { cond, body, r#else } => to_do!(),
            ast::Stmt::While { cond, body } => to_do!(),
            ast::Stmt::For { head, body } => to_do!(),
            ast::Stmt::Switch {
                cond,
                cases,
                default,
            } => to_do!(),
            ast::Stmt::Throw { expr } => to_do!(),
            ast::Stmt::Try {
                block,
                catch,
                finally,
            } => to_do!(),
            ast::Stmt::With { expr, stmt } => to_do!(),
            ast::Stmt::Break { label } => to_do!(),
            ast::Stmt::Continue { label } => to_do!(),
            ast::Stmt::Return { expr } => to_do!(),
            ast::Stmt::Labeled { label, stmt } => to_do!(),
            ast::Stmt::Function { func } => to_do!(),
            ast::Stmt::Class { class } => to_do!(),
            ast::Stmt::Debugger => to_do!(),
        }
    }
}
