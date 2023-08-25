use ast::NodeId;

use crate::{Compiler, Result};

impl<'a> Compiler<'a> {
    pub fn compile_stmt(&mut self, stmt: NodeId<ast::Stmt>) -> Result<Option<i8>> {
        match self.ast[stmt] {
            ast::Stmt::Block { list } => to_do!(),
            ast::Stmt::VariableDecl { kind, decl } => to_do!(),
            ast::Stmt::Empty => Ok(None),
            ast::Stmt::Expr { expr } => {
                let mut expr = Some(expr);
                let mut reg = None;
                while let Some(e) = expr {
                    let expr_item = &self.ast[e];
                    expr = expr_item.next;
                    reg = Some(self.compile_expr(expr_item.item)?);
                }
                Ok(reg)
            }
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
