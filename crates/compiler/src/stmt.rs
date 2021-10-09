use ast::{Expr, Literal, Stmt};
use runtime::instructions::Instruction;

use crate::{expr::ExprValue, register::Register, Compiler};
use std::alloc::Allocator;

impl<'a, A: Allocator + Clone> Compiler<'a, A> {
    pub(crate) fn compile_stmt(&mut self, stmt: &Stmt<A>) -> Option<Register> {
        match stmt {
            Stmt::Expr(x) => {
                let reg = self.compile_expressions(None, x).eval(self);
                self.registers.free_temp(reg);
                Some(reg)
            }
            Stmt::If(cond, r#if, r#else) => {
                self.compile_if(cond, r#if, r#else);
                None
            }
            Stmt::While(cond, block) => {
                self.compile_while(cond, block);
                None
            }
            Stmt::DoWhile(block, cond) => {
                self.compile_do_while(block, cond);
                None
            }
            Stmt::Let(symbol, expr) => {
                let reg = self.registers.alloc_symbol(*symbol);
                //TODO captured variables
                if let Some(x) = expr.as_ref() {
                    Some(self.compile_expr(Some(reg), x).eval(self))
                } else {
                    self.compile_literal(Some(reg), Literal::Undefined);
                    None
                }
            }
            Stmt::Const(symbol, expr) => {
                let reg = self.registers.alloc_symbol(*symbol);
                //TODO captured variables
                Some(self.compile_expr(Some(reg), expr).eval(self))
            }
            Stmt::Var(symbol, expr) => {
                if let Some(expr) = expr {
                    let expr = self.compile_expr(None, expr).eval(self);
                    let name = self.compile_literal(
                        None,
                        Literal::String(self.symbol_table.symbols()[*symbol].ident),
                    );
                    let reg = self.registers.alloc_temp();
                    self.instructions.push(Instruction::LoadGlobal {
                        dst: reg.0,
                        null: 0,
                    });
                    self.instructions.push(Instruction::IndexAssign {
                        obj: reg.0,
                        key: name.0,
                        val: expr.0,
                    });
                    self.registers.free_temp(reg);
                    self.registers.free_temp(expr);
                    self.registers.free_temp(name);
                    Some(expr)
                } else {
                    None
                }
            }
            Stmt::Block(_, stmts) => stmts.iter().map(|x| self.compile_stmt(x)).last().flatten(),
            _ => todo!(),
        }
    }

    pub fn compile_if(
        &mut self,
        cond: &Vec<Expr<A>, A>,
        r#if: &Stmt<A>,
        r#else: &Option<Box<Stmt<A>, A>>,
    ) {
        let expr = self.compile_expressions(None, cond);

        let patch_if = self.instructions.push(Instruction::JumpFalse {
            cond: expr.register.0,
            tgt: 1,
        });
        expr.true_list
            .into_iter()
            .for_each(|x| self.patch_jump(x, self.next_instruction_id()));

        self.compile_stmt(r#if).map(|r| self.registers.free_temp(r));
        if let Some(r#else) = r#else {
            let patch_else = self
                .instructions
                .push(Instruction::Jump { tgt: 1, null: 0 });
            expr.false_list
                .into_iter()
                .for_each(|x| self.patch_jump(x, self.next_instruction_id()));
            self.patch_jump(patch_if, self.next_instruction_id());
            self.compile_stmt(r#else)
                .map(|r| self.registers.free_temp(r));
            self.patch_jump(patch_else, self.next_instruction_id());
        } else {
            expr.false_list
                .into_iter()
                .for_each(|x| self.patch_jump(x, self.next_instruction_id()));
            self.patch_jump(patch_if, self.next_instruction_id());
        }
    }

    pub fn compile_while(&mut self, cond: &Vec<Expr<A>, A>, block: &Stmt<A>) {
        let before_cond = self.next_instruction_id();
        let expr = self.compile_expressions(None, cond);
        let patch_while = self.instructions.push(Instruction::JumpFalse {
            cond: expr.register.0,
            tgt: 1,
        });
        expr.true_list
            .into_iter()
            .for_each(|x| self.patch_jump(x, self.next_instruction_id()));

        self.compile_stmt(block)
            .map(|r| self.registers.free_temp(r));
        let before_jump = before_cond.0 as i32 - self.next_instruction_id().0 as i32;
        if before_jump as i16 as i32 == before_jump {
            self.instructions.push(Instruction::Jump {
                null: 0,
                tgt: before_jump as i16,
            });
        } else {
            self.instructions.push(Instruction::JumpL {
                nul0: 0,
                nul1: 0,
                tgt: before_jump,
            });
        }
        self.patch_jump(patch_while, self.next_instruction_id());
        expr.false_list
            .into_iter()
            .for_each(|x| self.patch_jump(x, self.next_instruction_id()));
    }

    pub fn compile_do_while(&mut self, block: &Stmt<A>, cond: &Vec<Expr<A>, A>) {
        let before_stmt = self.next_instruction_id();
        self.compile_stmt(block);
        let res = self.compile_expressions(None, cond);
        let before_jump = before_stmt.0 as i32 - self.next_instruction_id().0 as i32;
        if before_jump as i16 as i32 == before_jump {
            self.instructions.push(Instruction::JumpTrue {
                cond: res.register.0,
                tgt: before_jump as i16,
            });
        } else {
            self.instructions.push(Instruction::JumpTrueL {
                cond: res.register.0,
                null: 0,
                tgt: before_jump,
            });
        }
        res.true_list
            .into_iter()
            .for_each(|x| self.patch_jump(x, before_stmt));
        res.false_list
            .into_iter()
            .for_each(|x| self.patch_jump(x, self.next_instruction_id()));
    }
}
