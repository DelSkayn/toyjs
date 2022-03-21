use ast::{Expr, ForDecl, Literal, Params, ScopeId, Stmt};
use vm::instructions::Instruction;

use crate::{builder::FunctionId, expr::ExprValue, register::Register, Compiler};
use std::alloc::Allocator;

impl<'a, A: Allocator + Clone> Compiler<'a, A> {
    pub(crate) fn compile_stmt(&mut self, stmt: &'a Stmt<A>) -> Option<Register> {
        match stmt {
            Stmt::Empty => None,
            Stmt::Expr(x) => {
                let reg = self.compile_expressions(None, x).eval(self);
                self.builder.free_temp(reg);
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
            Stmt::Continue => {
                self.builder.push_continue();
                None
            }
            Stmt::Break => {
                self.builder.push_break();
                None
            }
            Stmt::Let(symbol, expr) => {
                let reg = self.builder.alloc_symbol(*symbol);
                if let Some(x) = expr.as_ref() {
                    Some(self.compile_expr(Some(reg), x).eval(self))
                } else {
                    self.compile_literal(Some(reg), Literal::Undefined);
                    None
                }
            }
            Stmt::Const(symbol, expr) => {
                let reg = self.builder.alloc_symbol(*symbol);
                Some(self.compile_expr(Some(reg), expr).eval(self))
            }
            Stmt::Var(symbol, expr) => {
                if self.symbol_table.is_symbol_local(*symbol) {
                    let reg = self.builder.alloc_symbol(*symbol);
                    if let Some(x) = expr.as_ref() {
                        Some(self.compile_expr(Some(reg), x).eval(self))
                    } else {
                        self.compile_literal(Some(reg), Literal::Undefined);
                        None
                    }
                } else if let Some(expr) = expr {
                    let expr = self.compile_expr(None, expr).eval(self);
                    let name = self.compile_literal(
                        None,
                        Literal::String(self.symbol_table.symbols()[*symbol].ident),
                    );
                    self.builder.push(Instruction::GlobalAssign {
                        key: name.0,
                        src: expr.0,
                    });
                    self.builder.free_temp(expr);
                    self.builder.free_temp(name);
                    Some(expr)
                } else {
                    None
                }
            }
            Stmt::Block(_, stmts) => stmts.iter().map(|x| self.compile_stmt(x)).last().flatten(),
            Stmt::Function(scope, symbol, params, stmts) => {
                let id = self.compile_function_decl(*scope, params, stmts);
                //TODO local functions stmts
                let fnc = self.builder.alloc_temp();
                self.builder.push(Instruction::LoadConstructor {
                    dst: fnc.0,
                    func: id.0,
                });
                let key = self.compile_literal(
                    None,
                    Literal::String(self.symbol_table.symbols()[*symbol].ident),
                );
                self.builder.push(Instruction::GlobalAssign {
                    key: key.0,
                    src: fnc.0,
                });
                self.builder.free_temp(key);
                self.builder.free_temp(fnc);
                None
            }
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    let reg = self.compile_expressions(None, expr).eval(self);
                    self.builder.free_temp(reg);
                    self.builder.push(Instruction::Return { ret: reg.0 });
                } else {
                    self.builder
                        .push(Instruction::ReturnUndefined { _ignore: () });
                }

                None
            }
            Stmt::Throw(expr) => {
                let reg = self.compile_expr(None, expr).eval(self);
                self.builder.free_temp(reg);
                self.builder.push(Instruction::Throw { src: reg.0 });
                None
            }
            Stmt::Try(r#try, catch, finally) => {
                let dst = catch
                    .as_ref()
                    .and_then(|x| x.binding.map(|x| self.builder.alloc_symbol(x)))
                    .unwrap_or_else(|| self.builder.alloc_temp());
                let instr = self.builder.push(Instruction::Try { tgt: 0, dst: dst.0 });
                self.compile_stmt(r#try);
                self.builder.push(Instruction::Untry { _ignore: () });
                if let Some(catch) = catch {
                    let catch_jmp = self.builder.push(Instruction::Jump { tgt: 0 });
                    self.builder
                        .patch_jump(instr, self.builder.next_instruction_id());
                    self.compile_stmt(&catch.stmt);
                    self.builder
                        .patch_jump(catch_jmp, self.builder.next_instruction_id());
                }
                if let Some(finally) = finally {
                    self.compile_stmt(finally);
                }

                None
            }
            x => {
                panic!("unimplemented statement ast: {:#?}", x);
            }
        }
    }

    pub fn compile_if(
        &mut self,
        cond: &'a [Expr<A>],
        r#if: &'a Stmt<A>,
        r#else: &'a Option<Box<Stmt<A>, A>>,
    ) {
        let expr = self.compile_expressions(None, cond);

        let patch_if = self.builder.push(Instruction::JumpFalse {
            cond: expr.register.0,
            tgt: 1,
        });
        expr.true_list.into_iter().for_each(|x| {
            self.builder
                .patch_jump(x, self.builder.next_instruction_id())
        });

        if let Some(r) = self.compile_stmt(r#if) {
            self.builder.free_temp(r);
        }
        if let Some(r#else) = r#else {
            let patch_else = self.builder.push(Instruction::Jump { tgt: 1 });
            expr.false_list.into_iter().for_each(|x| {
                self.builder
                    .patch_jump(x, self.builder.next_instruction_id())
            });
            self.builder
                .patch_jump(patch_if, self.builder.next_instruction_id());
            if let Some(r) = self.compile_stmt(r#else) {
                self.builder.free_temp(r);
            }
            self.builder
                .patch_jump(patch_else, self.builder.next_instruction_id());
        } else {
            expr.false_list.into_iter().for_each(|x| {
                self.builder
                    .patch_jump(x, self.builder.next_instruction_id())
            });
            self.builder
                .patch_jump(patch_if, self.builder.next_instruction_id());
        }
    }

    pub fn compile_while(&mut self, cond: &'a [Expr<A>], block: &'a Stmt<A>) {
        let before_cond = self.builder.next_instruction_id();
        let expr = self.compile_expressions(None, cond);
        let patch_while = self.builder.push(Instruction::JumpFalse {
            cond: expr.register.0,
            tgt: 1,
        });
        expr.true_list.into_iter().for_each(|x| {
            self.builder
                .patch_jump(x, self.builder.next_instruction_id())
        });

        self.builder.push_flow_scope();
        if let Some(r) = self.compile_stmt(block) {
            self.builder.free_temp(r);
        }
        let flow_scope = self.builder.pop_flow_scope();
        let back_jump = self.builder.push(Instruction::Jump { tgt: 1 });

        // patch jump back to condition
        self.builder.patch_jump(back_jump, before_cond);
        // patch jump from condition
        self.builder
            .patch_jump(patch_while, self.builder.next_instruction_id());
        expr.false_list.into_iter().for_each(|x| {
            self.builder
                .patch_jump(x, self.builder.next_instruction_id())
        });

        flow_scope
            .patch_continue
            .into_iter()
            .for_each(|x| self.builder.patch_jump(x, before_cond));

        flow_scope.patch_break.into_iter().for_each(|x| {
            self.builder
                .patch_jump(x, self.builder.next_instruction_id())
        });
    }

    pub fn compile_do_while(&mut self, block: &'a Stmt<A>, cond: &'a [Expr<A>]) {
        let before_stmt = self.builder.next_instruction_id();

        self.builder.push_flow_scope();
        self.compile_stmt(block);
        let flow_scope = self.builder.pop_flow_scope();

        flow_scope.patch_continue.into_iter().for_each(|x| {
            self.builder
                .patch_jump(x, self.builder.next_instruction_id())
        });

        let res = self.compile_expressions(None, cond);
        let back_jump = self.builder.push(Instruction::JumpTrue {
            cond: res.register.0,
            tgt: 1,
        });
        self.builder.patch_jump(back_jump, before_stmt);
        res.true_list
            .into_iter()
            .for_each(|x| self.builder.patch_jump(x, before_stmt));
        res.false_list.into_iter().for_each(|x| {
            self.builder
                .patch_jump(x, self.builder.next_instruction_id())
        });

        flow_scope.patch_break.into_iter().for_each(|x| {
            self.builder
                .patch_jump(x, self.builder.next_instruction_id())
        });
    }

    pub fn compile_function_decl(
        &mut self,
        scope: ScopeId,
        params: &'a Params<A>,
        block: &'a [Stmt<A>],
    ) -> FunctionId {
        let id = self.builder.push_function(scope, params);
        for s in block.iter() {
            self.compile_stmt(s);
        }

        match self.builder.instructions().last() {
            Some(Instruction::Return { .. }) => {}
            Some(Instruction::ReturnUndefined { .. }) => {}
            Some(Instruction::Throw { .. }) => {}
            _ => {
                self.builder
                    .push(Instruction::ReturnUndefined { _ignore: () });
            }
        }
        self.builder.pop_function();
        id
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
                self.compile_stmt(x);
            }
            ForDecl::Expr(x) => {
                let reg = self.compile_expr(None, x).eval(self);
                self.builder.free_temp(reg);
            }
        }
        let before_cond = self.builder.next_instruction_id();
        let cond = self.compile_expr(None, cond);
        let patch_cond = self.builder.push(Instruction::JumpFalse {
            cond: cond.register.0,
            tgt: 1,
        });

        cond.true_list.into_iter().for_each(|x| {
            self.builder
                .patch_jump(x, self.builder.next_instruction_id())
        });

        self.builder.push_flow_scope();
        self.compile_stmt(block);
        let flow_scope = self.builder.pop_flow_scope();

        flow_scope.patch_continue.into_iter().for_each(|x| {
            self.builder
                .patch_jump(x, self.builder.next_instruction_id())
        });

        let reg = self.compile_expr(None, post).eval(self);
        self.builder.free_temp(reg);
        let back_jump = self.builder.push(Instruction::Jump { tgt: 1 });
        self.builder.patch_jump(back_jump, before_cond);
        self.builder
            .patch_jump(patch_cond, self.builder.next_instruction_id());
        cond.false_list.into_iter().for_each(|x| {
            self.builder
                .patch_jump(x, self.builder.next_instruction_id())
        });

        flow_scope.patch_break.into_iter().for_each(|x| {
            self.builder
                .patch_jump(x, self.builder.next_instruction_id())
        })
    }
}
