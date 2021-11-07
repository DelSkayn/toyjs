use ast::{Expr, ForDecl, Literal, Params, Stmt};
use vm::instructions::Instruction;

use crate::{
    expr::ExprValue,
    lexical_info::{ArgAllocInfo, SymbolInfo},
    register::Register,
    Compiler,
};
use std::alloc::Allocator;

impl<'a, A: Allocator + Clone> Compiler<'a, A> {
    pub(crate) fn compile_stmt(&mut self, stmt: &'a Stmt<A>) -> Option<Register> {
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
            Stmt::For(decl, cond, post, block) => {
                self.compile_for(decl, cond, post, block);
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
            Stmt::Function(scope, symbol, params, stmts) => {
                let id = self.push_pending_function(*scope, params, stmts);
                //TODO local functions stmts
                let tmp = self.registers.alloc_temp();
                let glob = self.registers.alloc_temp();
                if id.requires_long() {
                    self.instructions.push(Instruction::LoadFunctionL {
                        dst: tmp.0,
                        null: 0,
                        func: id.0,
                    });
                } else {
                    self.instructions.push(Instruction::LoadFunction {
                        dst: tmp.0,
                        func: id.0 as u16,
                    });
                }
                self.instructions.push(Instruction::LoadGlobal {
                    dst: glob.0,
                    null: 0,
                });
                let key = self.compile_literal(
                    None,
                    Literal::String(self.symbol_table.symbols()[*symbol].ident),
                );
                self.instructions.push(Instruction::IndexAssign {
                    obj: glob.0,
                    key: key.0,
                    val: tmp.0,
                });
                self.registers.free_temp(key);
                self.registers.free_temp(glob);
                self.registers.free_temp(tmp);
                None
            }
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    let reg = self.compile_expressions(None, expr).eval(self);
                    self.registers.free_temp(reg);
                    self.instructions.push(Instruction::Return {
                        ret: reg.0,
                        null: 0,
                    });
                } else {
                    self.instructions
                        .push(Instruction::ReturnUndefined { nul0: 0, nul1: 0 });
                }

                None
            }
            _ => todo!(),
        }
    }

    pub fn compile_if(
        &mut self,
        cond: &'a Vec<Expr<A>, A>,
        r#if: &'a Stmt<A>,
        r#else: &'a Option<Box<Stmt<A>, A>>,
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

    pub fn compile_while(&mut self, cond: &'a Vec<Expr<A>, A>, block: &'a Stmt<A>) {
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
        let back_jump = self
            .instructions
            .push(Instruction::Jump { null: 0, tgt: 1 });
        self.patch_jump(back_jump, before_cond);
        self.patch_jump(patch_while, self.next_instruction_id());
        expr.false_list
            .into_iter()
            .for_each(|x| self.patch_jump(x, self.next_instruction_id()));
    }

    pub fn compile_do_while(&mut self, block: &'a Stmt<A>, cond: &'a Vec<Expr<A>, A>) {
        let before_stmt = self.next_instruction_id();
        self.compile_stmt(block);
        let res = self.compile_expressions(None, cond);
        let back_jump = self.instructions.push(Instruction::JumpTrue {
            cond: res.register.0,
            tgt: 1,
        });
        self.patch_jump(back_jump, before_stmt);
        res.true_list
            .into_iter()
            .for_each(|x| self.patch_jump(x, before_stmt));
        res.false_list
            .into_iter()
            .for_each(|x| self.patch_jump(x, self.next_instruction_id()));
    }

    pub fn compile_for(
        &mut self,
        decl: &'a ForDecl<A>,
        cond: &'a Expr<A>,
        post: &'a Expr<A>,
        block: &'a Stmt<A>,
    ) {
        match decl {
            ForDecl::Stmt(x) => {
                self.compile_stmt(&x);
            }
            ForDecl::Expr(x) => {
                let reg = self.compile_expr(None, x).eval(self);
                self.registers.free_temp(reg);
            }
        }
        let before_cond = self.next_instruction_id();
        let cond = self.compile_expr(None, cond);
        let patch_cond = self.instructions.push(Instruction::JumpFalse {
            cond: cond.register.0,
            tgt: 1,
        });
        cond.true_list
            .into_iter()
            .for_each(|x| self.patch_jump(x, self.next_instruction_id()));
        self.compile_stmt(block);
        let reg = self.compile_expr(None, post).eval(self);
        self.registers.free_temp(reg);
        let back_jump = self
            .instructions
            .push(Instruction::Jump { tgt: 1, null: 0 });
        self.patch_jump(back_jump, before_cond);
        self.patch_jump(patch_cond, self.next_instruction_id());
        cond.false_list
            .into_iter()
            .for_each(|x| self.patch_jump(x, self.next_instruction_id()));
    }

    pub fn compile_params(&mut self, params: &'a Params<A>) {
        for (i, p) in params.0.iter().enumerate() {
            if i >= 16 {
                todo!();
            }
            match self.lexical_info.symbol_info[*p] {
                SymbolInfo::Argument(ref mut alloc) => {
                    *alloc = ArgAllocInfo::Register(self.registers.alloc_arg(*p))
                }
                ref x => panic!(
                    "argument symbol is not an argument in its corresponding info: {:?}",
                    x
                ),
            }
        }
        if let Some(_) = params.1 {
            todo!()
        }
    }
}
