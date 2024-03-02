use ast::{ListHead, NodeId};
use bc::{Instruction, LongOffset, Reg};

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
                self.free_tmp_register(tmp);
                Ok(Some(tmp))
            }
            ast::Stmt::DoWhile { body, cond } => {
                let before_block = self.next_instruction()?;
                self.compile_stmt(body)?;
                let mut cond = self.compile_exprs(cond)?;
                let cond_reg = cond.to_cond_register(self)?;
                cond.patch_true_jumps_to(self, before_block)?;
                let instr = self.emit(Instruction::LongJumpFalse {
                    cond: cond_reg,
                    dst: LongOffset(0),
                })?;
                self.patch_jump(instr, before_block)?;
                let next = self.next_instruction()?;
                cond.patch_false_jumps_to(self, next)?;
                Ok(None)
            }
            ast::Stmt::If { cond, body, r#else } => {
                let mut cond = self.compile_exprs(cond)?;
                let cond_reg = cond.to_cond_register(self)?;
                let cond_jump = self.emit(Instruction::LongJumpFalse {
                    cond: cond_reg,
                    dst: LongOffset(0),
                })?;
                let next = self.next_instruction()?;
                cond.patch_true_jumps_to(self, next)?;
                self.compile_stmt(body)?;
                if let Some(r#else) = r#else {
                    let else_jump = self.emit(Instruction::LongJump { dst: LongOffset(0) })?;
                    let next_instr = self.next_instruction()?;
                    cond.patch_false_jumps_to(self, next_instr)?;
                    self.patch_jump(cond_jump, next_instr)?;
                    self.compile_stmt(r#else)?;
                    let next_instr = self.next_instruction()?;
                    self.patch_jump(else_jump, next_instr)?;
                } else {
                    let next_instr = self.next_instruction()?;
                    cond.patch_false_jumps_to(self, next_instr)?;
                    self.patch_jump(cond_jump, next_instr)?;
                }
                Ok(None)
            }
            ast::Stmt::While { cond, body } => {
                let before_cond = self.next_instruction()?;
                let mut cond = self.compile_exprs(cond)?;
                let cond_reg = cond.to_cond_register(self)?;
                let cond_jump = self.emit(Instruction::LongJumpFalse {
                    cond: cond_reg,
                    dst: LongOffset(0),
                })?;
                let next = self.next_instruction()?;
                cond.patch_true_jumps_to(self, next)?;
                self.compile_stmt(body)?;
                let back_jump = self.emit(Instruction::LongJump { dst: LongOffset(0) })?;
                self.patch_jump(back_jump, before_cond)?;
                let after_loop = self.next_instruction()?;
                cond.patch_false_jumps_to(self, after_loop)?;
                self.patch_jump(cond_jump, after_loop)?;
                Ok(None)
            }
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
                    self.free_tmp_register(res);
                    self.emit(Instruction::Ret { src: res })?;
                    Ok(None)
                } else {
                    self.emit(Instruction::RetUndefined {})?;
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
                self.store_symbol(sym_id, expr)?;
                // TODO: Expression
                Ok(None)
            }
            ast::IdentOrPattern::Pattern(_) => todo!(),
        }
    }
}
