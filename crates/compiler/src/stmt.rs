use ast::{Expr, Literal, Stmt};
use runtime::instructions::Instruction;

use crate::{register::Register, Compiler};
use std::alloc::Allocator;

impl<'a, A: Allocator + Clone> Compiler<'a, A> {
    pub(crate) fn compile_stmt(&mut self, stmt: &Stmt<A>) -> Option<Register> {
        match stmt {
            Stmt::Expr(x) => Some(
                x.iter()
                    .map(|x| {
                        let place = self.compile_expr(None, x).eval(self);
                        self.registers.free_temp(place);
                        place
                    })
                    .last()
                    .expect("expression node did not have, atleast, a single expression"),
            ),
            Stmt::If(cond, r#if, r#else) => {
                self.compile_if(cond, r#if, r#else);
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
        for expr in cond[..cond.len() - 1].iter() {
            let place = self.compile_expr(None, expr).eval(self);
            self.registers.free_temp(place);
        }
        let expr = self.compile_expr(
            None,
            cond.last()
                .expect("if condition did not have a single expression"),
        );

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
}
