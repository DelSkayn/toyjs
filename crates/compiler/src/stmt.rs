use ast::{ListHead, NodeId};
use bc::Reg;

use crate::{Compiler, Result};

impl<'a> Compiler<'a> {
    pub fn compile_stmt(&mut self, stmt: NodeId<ast::Stmt>) -> Result<Option<Reg>> {
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
            ast::Stmt::Expr { expr } => self.compile_exprs(None, expr).map(Some),
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
            ast::Stmt::Return { expr } => {
                if let Some(expr) = expr {
                    let res = self.compile_exprs(None, expr)?;
                    self.instructions.push(bc::Instruction::Ret { src: res });
                    Ok(None)
                } else {
                    self.instructions.push(bc::Instruction::RetUndefind {});
                    Ok(None)
                }
            }
            ast::Stmt::Labeled { label, stmt } => to_do!(),
            ast::Stmt::Function { func } => to_do!(),
            ast::Stmt::Class { class } => to_do!(),
            ast::Stmt::Debugger => Ok(None),
        }
    }
}
