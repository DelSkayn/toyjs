use ast::{ArrowBody, Case, Expr, ForDecl, Literal, Params, ScopeId, Stmt, SymbolId};
use vm::instructions::Instruction;

use crate::{
    builder::FunctionId,
    expr::{AssignmentTarget, ExprValue},
    register::Register,
    Compiler,
};
use std::alloc::Allocator;

impl<'gc,'own, A: Allocator + Clone> Compiler<'gc,'own, A> {
    pub(crate) fn compile_stmt(&mut self, stmt: &'gc Stmt<A>) -> Option<Register> {
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
            Stmt::Switch(cond, clauses, r#default) => {
                self.compile_switch(cond, clauses, r#default.as_deref());
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
            Stmt::For(ast::For::CStyle(decl, cond, post, block)) => {
                self.compile_for(decl, cond.as_deref(), post.as_deref(), block);
                None
            }
            Stmt::For(ast::For::ForIn(decl, expr, stmt)) => {
                self.compile_for_in(*decl, expr, &*stmt);
                None
            }
            Stmt::For(ast::For::ForOf(_decl, _expr, _stmt)) => {
                todo!("for of");
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
            Stmt::Var(symbols) => {
                let mut reg = None;
                for (symbol, expr) in symbols {
                    let symbol = self.builder.symbol_table.resolve_symbol(*symbol);
                    if self.builder.symbol_table.is_symbol_local(symbol) {
                        let dst = self.builder.alloc_symbol(symbol);
                        if let Some(x) = expr.as_ref() {
                            reg = Some(self.compile_expr(Some(dst), x).eval(self));
                        } else {
                            self.compile_literal(Some(dst), Literal::Undefined);
                            reg = None;
                        }
                    } else if let Some(expr) = expr {
                        let expr = self.compile_expr(None, expr).eval(self);
                        let name = self
                            .compile_atom(None, self.builder.symbol_table.symbols()[symbol].ident);
                        self.builder.push(Instruction::GlobalAssign {
                            key: name.0,
                            src: expr.0,
                        });
                        self.builder.free_temp(expr);
                        self.builder.free_temp(name);
                        reg = Some(expr);
                    } else {
                        reg = None;
                    }
                }
                reg
            }
            Stmt::Block(_, stmts) => stmts.iter().map(|x| self.compile_stmt(x)).last().flatten(),
            Stmt::Function(scope, symbol, params, stmts) => {
                let id = self.compile_function_decl(*scope, params, stmts);
                //TODO: local functions stmts
                let fnc = self.builder.alloc_temp();
                self.builder.push(Instruction::LoadConstructor {
                    dst: fnc.0,
                    func: id.0,
                });
                let key =
                    self.compile_atom(None, self.builder.symbol_table.symbols()[*symbol].ident);
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
                self.builder.push(Instruction::Untry { dst: dst.0 });
                if let Some(catch) = catch {
                    let catch_jmp = self.builder.push(Instruction::Jump { tgt: 0 });
                    self.builder
                        .patch_jump(instr, self.builder.next_instruction_id());
                    self.compile_stmt(&catch.stmt);
                    self.builder
                        .patch_jump(catch_jmp, self.builder.next_instruction_id());
                }
                if let Some(finally) = finally {
                    if catch.is_none() {
                        self.builder
                            .patch_jump(instr, self.builder.next_instruction_id());
                    }
                    self.compile_stmt(finally);
                    self.builder.push(Instruction::Throw { src: dst.0 });
                }
                self.builder.free_temp(dst);

                None
            }
        }
    }

    pub fn compile_if(
        &mut self,
        cond: &'gc [Expr<A>],
        r#if: &'gc Stmt<A>,
        r#else: &'gc Option<Box<Stmt<A>, A>>,
    ) {
        let expr = self.compile_expressions(None, cond);

        let patch_if = self.builder.push(Instruction::JumpFalse {
            cond: expr.register.0,
            tgt: 1,
        });
        self.builder.free_temp(expr.register);
        expr.true_list.into_iter().for_each(|x| {
            self.builder
                .patch_jump(x, self.builder.next_instruction_id());
        });

        if let Some(r) = self.compile_stmt(r#if) {
            self.builder.free_temp(r);
        }
        if let Some(r#else) = r#else {
            let patch_else = self.builder.push(Instruction::Jump { tgt: 1 });
            expr.false_list.into_iter().for_each(|x| {
                self.builder
                    .patch_jump(x, self.builder.next_instruction_id());
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
                    .patch_jump(x, self.builder.next_instruction_id());
            });
            self.builder
                .patch_jump(patch_if, self.builder.next_instruction_id());
        }
    }

    pub fn compile_switch(
        &mut self,
        cond: &'gc [Expr<A>],
        cases: &'gc [Case<A>],
        r#default: Option<&'gc [Stmt<A>]>,
    ) {
        let cond = self.compile_expressions(None, cond).eval(self);
        // Compile condition jumps
        let mut jumps = Vec::new_in(self.alloc.clone());
        for case in cases.iter() {
            let v = self.compile_expr(None, &case.expr).eval(self);
            self.builder.free_temp(v);
            self.builder.push(Instruction::SEqual {
                dst: v.0,
                left: cond.0,
                righ: v.0,
            });
            jumps.push(
                self.builder
                    .push(Instruction::JumpTrue { cond: v.0, tgt: 0 }),
            );
        }
        let after_cond = self.builder.push(Instruction::Jump { tgt: 0 });
        self.builder.free_temp(cond);
        // Compile statements.
        self.builder.push_flow_scope();
        for (c, jump) in cases.iter().zip(jumps) {
            self.builder
                .patch_jump(jump, self.builder.next_instruction_id());
            for s in &c.stmts {
                self.compile_stmt(s);
            }
        }
        self.builder
            .patch_jump(after_cond, self.builder.next_instruction_id());
        if let Some(stmts) = r#default {
            for s in stmts {
                self.compile_stmt(s);
            }
        }
        let flow = self.builder.pop_flow_scope();
        for j in flow.patch_break {
            self.builder
                .patch_jump(j, self.builder.next_instruction_id());
        }
    }

    pub fn compile_while(&mut self, cond: &'gc [Expr<A>], block: &'gc Stmt<A>) {
        let before_cond = self.builder.next_instruction_id();
        let expr = self.compile_expressions(None, cond);
        let patch_while = self.builder.push(Instruction::JumpFalse {
            cond: expr.register.0,
            tgt: 1,
        });
        self.builder.free_temp(expr.register);
        expr.true_list.into_iter().for_each(|x| {
            self.builder
                .patch_jump(x, self.builder.next_instruction_id());
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
                .patch_jump(x, self.builder.next_instruction_id());
        });

        flow_scope
            .patch_continue
            .into_iter()
            .for_each(|x| self.builder.patch_jump(x, before_cond));

        flow_scope.patch_break.into_iter().for_each(|x| {
            self.builder
                .patch_jump(x, self.builder.next_instruction_id());
        });
    }

    pub fn compile_do_while(&mut self, block: &'gc Stmt<A>, cond: &'gc [Expr<A>]) {
        let before_stmt = self.builder.next_instruction_id();

        self.builder.push_flow_scope();
        self.compile_stmt(block);
        let flow_scope = self.builder.pop_flow_scope();

        flow_scope.patch_continue.into_iter().for_each(|x| {
            self.builder
                .patch_jump(x, self.builder.next_instruction_id());
        });

        let res = self.compile_expressions(None, cond);
        let back_jump = self.builder.push(Instruction::JumpTrue {
            cond: res.register.0,
            tgt: 1,
        });
        self.builder.free_temp(res.register);
        self.builder.patch_jump(back_jump, before_stmt);
        res.true_list
            .into_iter()
            .for_each(|x| self.builder.patch_jump(x, before_stmt));
        res.false_list.into_iter().for_each(|x| {
            self.builder
                .patch_jump(x, self.builder.next_instruction_id());
        });

        flow_scope.patch_break.into_iter().for_each(|x| {
            self.builder
                .patch_jump(x, self.builder.next_instruction_id());
        });
    }

    pub fn compile_function_decl(
        &mut self,
        scope: ScopeId,
        params: &'gc Params<A>,
        block: &'gc [Stmt<A>],
    ) -> FunctionId {
        let id = self.builder.push_function(scope, params);
        for s in block.iter() {
            self.compile_stmt(s);
        }

        match self.builder.instructions().last() {
            Some(Instruction::Return { .. })
            | Some(Instruction::ReturnUndefined { .. })
            | Some(Instruction::Throw { .. }) => {}
            _ => {
                self.builder
                    .push(Instruction::ReturnUndefined { _ignore: () });
            }
        }
        self.builder.pop_function();
        id
    }

    pub fn compile_arrow_function_decl(
        &mut self,
        scope: ScopeId,
        params: &'gc Params<A>,
        body: &'gc ArrowBody<A>,
    ) -> FunctionId {
        let id = self.builder.push_function(scope, params);
        match body {
            ArrowBody::Block(block) => {
                for s in block {
                    self.compile_stmt(s);
                }

                match self.builder.instructions().last() {
                    Some(Instruction::Return { .. })
                    | Some(Instruction::ReturnUndefined { .. })
                    | Some(Instruction::Throw { .. }) => {}
                    _ => {
                        self.builder
                            .push(Instruction::ReturnUndefined { _ignore: () });
                    }
                }
            }
            ArrowBody::Expr(x) => {
                let reg = self.compile_expr(None, x).eval(self);
                self.builder.free_temp(reg);
                self.builder.push(Instruction::Return { ret: reg.0 });
            }
        }
        self.builder.pop_function();
        id
    }

    pub fn compile_for(
        &mut self,
        decl: &'gc Option<ForDecl<A>>,
        cond: Option<&'gc [Expr<A>]>,
        post: Option<&'gc [Expr<A>]>,
        block: &'gc Stmt<A>,
    ) {
        if let Some(decl) = decl {
            match decl {
                ForDecl::Stmt(x) => {
                    self.compile_stmt(x);
                }
                ForDecl::Expr(x) => {
                    let reg = self.compile_expr(None, x).eval(self);
                    self.builder.free_temp(reg);
                }
            }
        }
        let before_cond = self.builder.next_instruction_id();
        let patch_cond = if let Some(cond) = cond {
            let cond = self.compile_expressions(None, cond);
            self.builder.free_temp(cond.register);
            let patch_cond = self.builder.push(Instruction::JumpFalse {
                cond: cond.register.0,
                tgt: 1,
            });
            self.builder.free_temp(cond.register);

            for x in &cond.true_list {
                self.builder
                    .patch_jump(*x, self.builder.next_instruction_id())
            }
            Some((patch_cond, cond))
        } else {
            None
        };

        self.builder.push_flow_scope();
        self.compile_stmt(block);
        let flow_scope = self.builder.pop_flow_scope();

        for x in flow_scope.patch_continue {
            self.builder
                .patch_jump(x, self.builder.next_instruction_id());
        }

        if let Some(post) = post {
            let reg = self.compile_expressions(None, post).eval(self);
            self.builder.free_temp(reg);
        }
        let back_jump = self.builder.push(Instruction::Jump { tgt: 0 });
        self.builder.patch_jump(back_jump, before_cond);
        if let Some((patch_cond, cond)) = patch_cond {
            self.builder
                .patch_jump(patch_cond, self.builder.next_instruction_id());
            for x in cond.false_list {
                self.builder
                    .patch_jump(x, self.builder.next_instruction_id());
            }
        }

        for x in flow_scope.patch_break {
            self.builder
                .patch_jump(x, self.builder.next_instruction_id());
        }
    }

    pub fn compile_for_in(&mut self, decl: SymbolId, expr: &'gc [Expr<A>], stmt: &'gc Stmt<A>) {
        let decl = self.builder.symbol_table.resolve_symbol(decl);
        let tgt = AssignmentTarget::from_symbol(decl);
        let res = self.compile_expressions(None, expr).eval(self);
        self.builder.free_temp(res);
        let iter = self.builder.alloc_temp();
        self.builder.push(Instruction::IterHead {
            dst: iter.0,
            obj: res.0,
        });
        let jump_head = self.builder.push(Instruction::Jump { tgt: 0 });

        let tgt_reg = tgt
            .placement(self)
            .unwrap_or_else(|| self.builder.alloc_temp());
        let start = self.builder.push(Instruction::Iter {
            dst: tgt_reg.0,
            obj: iter.0,
        });
        let jump_iter = self.builder.push(Instruction::Jump { tgt: 0 });
        tgt.compile_assign(self, tgt_reg);

        self.builder.push_flow_scope();
        self.compile_stmt(stmt);
        let flow_scope = self.builder.pop_flow_scope();

        for x in flow_scope.patch_continue {
            self.builder.patch_jump(x, start);
        }

        let j = self.builder.push(Instruction::Jump { tgt: 0 });
        self.builder.patch_jump(j, start);

        self.builder.free_temp(iter);
        self.builder.free_temp(tgt_reg);

        for x in flow_scope.patch_break {
            self.builder
                .patch_jump(x, self.builder.next_instruction_id());
        }

        self.builder
            .patch_jump(jump_head, self.builder.next_instruction_id());
        self.builder
            .patch_jump(jump_iter, self.builder.next_instruction_id());
    }
}
