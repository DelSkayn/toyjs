use ast::{ListHead, NodeId};
use bc::Reg;

use crate::{variables::Kind, Compiler, Error, Limits, Result};

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
            ast::Stmt::VariableDecl { decl, .. } => {
                let mut cur = decl;
                let mut res;
                loop {
                    res = self.compile_variable_decl(self.ast[cur].item)?;
                    let Some(next) = self.ast[decl].next else {
                        break;
                    };
                    cur = next;
                }
                Ok(res)
            }
            ast::Stmt::Empty => Ok(None),
            ast::Stmt::Expr { expr } => {
                let tmp = self.compile_exprs(expr)?.to_register(self)?;
                self.registers.free_if_tmp(tmp);
                Ok(Some(tmp))
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
            ast::Stmt::Return { expr } => {
                if let Some(expr) = expr {
                    let res = self.compile_exprs(expr)?.to_register(self)?;
                    self.registers.free_if_tmp(res);
                    self.emit(bc::Instruction::Ret { src: res })?;
                    Ok(None)
                } else {
                    self.emit(bc::Instruction::RetUndefined {})?;
                    Ok(None)
                }
            }
            ast::Stmt::Labeled { label, stmt } => to_do!(),
            ast::Stmt::Function { func } => to_do!(),
            ast::Stmt::Class { class } => to_do!(),
            ast::Stmt::Debugger => Ok(None),
        }
    }

    pub fn compile_variable_decl(
        &mut self,
        decl: NodeId<ast::VariableDecl>,
    ) -> Result<Option<Reg>> {
        let Some(expr) = self.ast[decl].initializer else {
            return Ok(None);
        };
        let expr = self.compile_expr(expr)?;
        let decl = self.ast[decl].decl;
        match self.ast[decl] {
            ast::IdentOrPattern::Ident(sym) => {
                let sym_id = self.variables.symbol_of_ast(sym);
                match self.variables.symbols()[sym_id].kind {
                    Kind::Function | Kind::Let | Kind::Const => {
                        if let Some(until) = self.variables.last_use_of(sym_id) {
                            let reg = self
                                .registers
                                .alloc_symbol(sym_id, until)
                                .ok_or(Error::ExceededLimits(Limits::Registers))?;

                            expr.assign_to_reg(self, reg)?;

                            Ok(Some(reg))
                        } else {
                            expr.ignore(self)?;
                            Ok(None)
                        }
                    }
                    Kind::Global | Kind::Unresolved => {
                        to_do!()
                    }
                    Kind::Arg => unreachable!(),
                }
            }
            ast::IdentOrPattern::Pattern(_) => todo!(),
        }
    }
}
