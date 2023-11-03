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
            ast::Stmt::VariableDecl { mut decl, .. } => {
                let res = None;
                loop {
                    let item = self.ast[decl].item;
                    if let Some(x) = self.ast[item].initializer {
                        let decl = self.ast[item].decl;
                        //let reg = self.compile_expr(x, x)?;
                        //self.registers.store(&self.ast, ident);
                        //res = Some(reg);
                        todo!()
                    };
                    let Some(x) = self.ast[decl].next else { break };
                    decl = x
                }
                Ok(res)
            }
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
                    self.instructions.push(bc::Instruction::RetUndefined {});
                    Ok(None)
                }
            }
            ast::Stmt::Labeled { label, stmt } => to_do!(),
            ast::Stmt::Function { func } => to_do!(),
            ast::Stmt::Class { class } => to_do!(),
            ast::Stmt::Debugger => Ok(None),
        }
    }

    fn compile_decl(&mut self, decl: NodeId<ast::IdentOrPattern>, from: Option<Reg>) {}
}
