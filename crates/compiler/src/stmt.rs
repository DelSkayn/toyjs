use ast::{
    ArrowBody, Binding, BindingElement, BindingRestElement, CForDecl, Case, Define, Expr,
    ForInOfDecl, Literal, Params, ScopeId, Stmt, SymbolId, SymbolOrBinding,
};
use vm::instructions::Instruction;

use crate::{
    builder::FunctionId,
    expr::{AssignmentTarget, ExprValue},
    register::Register,
    Compiler,
};
use std::alloc::Allocator;

const NO_DECL_PANIC: &str = "declarations should atleast have a single declared variable";

impl<'a, 'rt, 'cell, A: Allocator + Clone> Compiler<'a, 'rt, 'cell, A> {
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
                self.compile_for_in(decl, expr, &*stmt);
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
            Stmt::Let(x) => x
                .iter()
                .map(|d| self.compile_define(d))
                .last()
                .expect(NO_DECL_PANIC),
            Stmt::Const(x) => x
                .iter()
                .map(|d| self.compile_define(d))
                .last()
                .expect(NO_DECL_PANIC),
            Stmt::Var(symbols) => symbols
                .iter()
                .map(|d| self.compile_define(d))
                .last()
                .expect(NO_DECL_PANIC),
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
                    .and_then(|x| {
                        if let Some(ast::SymbolOrBinding::Single(x)) = x.binding {
                            Some(self.builder.alloc_symbol(x))
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|| self.builder.alloc_temp());
                let instr = self.builder.push(Instruction::Try { tgt: 0, dst: dst.0 });
                self.compile_stmt(r#try);
                self.builder.push(Instruction::Untry { dst: dst.0 });
                if let Some(catch) = catch {
                    let catch_jmp = self.builder.push(Instruction::Jump { tgt: 0 });
                    self.builder
                        .patch_jump(instr, self.builder.next_instruction_id());

                    match catch.binding {
                        None => {}
                        Some(ast::SymbolOrBinding::Single(_)) => {}
                        Some(ast::SymbolOrBinding::Binding(ref x)) => {
                            self.compile_binding(x, dst);
                        }
                    }
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

    pub fn compile_define(&mut self, define: &'a Define<A>) -> Option<Register> {
        match *define {
            ast::Define::Single { symbol, ref init } => {
                if let Some(x) = init.as_ref() {
                    let tgt = AssignmentTarget::Variable(symbol);
                    let place = tgt.placement(self);
                    let res = self.compile_expr(place, x).eval(self);
                    tgt.compile_assign(self, res);
                    self.builder.free_temp(res);
                    Some(res)
                } else {
                    let tgt = AssignmentTarget::Variable(symbol);
                    let place = tgt.placement(self);
                    let res = self.compile_literal(place, Literal::Undefined);
                    tgt.compile_assign(self, res);
                    self.builder.free_temp(res);
                    None
                }
            }
            ast::Define::Binding {
                ref binding,
                ref init,
            } => {
                let expr = self.compile_expr(None, init).eval(self);
                self.compile_binding(binding, expr);
                self.builder.free_temp(expr);
                Some(expr)
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
        cond: &'a [Expr<A>],
        cases: &'a [Case<A>],
        r#default: Option<&'a [Stmt<A>]>,
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

    pub fn compile_while(&mut self, cond: &'a [Expr<A>], block: &'a Stmt<A>) {
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

    pub fn compile_do_while(&mut self, block: &'a Stmt<A>, cond: &'a [Expr<A>]) {
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
        params: &'a Params<A>,
        block: &'a [Stmt<A>],
    ) -> FunctionId {
        let id = self.builder.push_function(scope);

        let bindings: Vec<_> = params
            .0
            .iter()
            .enumerate()
            .filter_map(|(idx, p)| {
                if idx >= 16 {
                    todo!("more then 16 arguments")
                }
                match p {
                    ast::SymbolOrBinding::Single(x) => {
                        self.builder.registers().alloc_arg(Some(*x));
                        None
                    }
                    ast::SymbolOrBinding::Binding(ref b) => {
                        let reg = self.builder.registers().alloc_arg(None);
                        Some((reg, b))
                    }
                }
            })
            .collect();

        for (reg, b) in bindings {
            self.compile_binding(b, reg);
        }

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

    pub fn compile_binding(&mut self, binding: &'a Binding<A>, obj: Register) {
        match binding {
            Binding::Array(ref elements, ref rem) => {
                for (idx, e) in elements.iter().enumerate() {
                    if let Some(binding) = e {
                        let key =
                            self.compile_literal(None, Literal::Integer(idx.try_into().unwrap()));
                        self.compile_binding_element(binding, key, obj)
                    }
                }

                match rem {
                    BindingRestElement::None => {}
                    BindingRestElement::Binding(_) => todo!(),
                    BindingRestElement::Single(_) => todo!(),
                }
            }
            Binding::Object(properties, rem) => {
                for p in properties.iter() {
                    match *p {
                        ast::BindingProperty::Single(sym, ref expr) => {
                            let tgt = AssignmentTarget::Variable(sym);
                            let key = self
                                .compile_atom(None, self.builder.symbol_table.symbols()[sym].ident);
                            let dst = tgt
                                .placement(self)
                                .unwrap_or_else(|| self.builder.alloc_temp());
                            self.builder.free_temp(key);
                            self.builder.push(Instruction::Index {
                                dst: dst.0,
                                obj: obj.0,
                                key: key.0,
                            });
                            if let Some(ref expr) = *expr {
                                let cond = self.builder.alloc_temp();
                                self.builder.free_temp(cond);
                                self.builder.push(Instruction::IsUndefined {
                                    dst: cond.0,
                                    op: dst.0,
                                });
                                let before = self.builder.push(Instruction::JumpFalse {
                                    cond: cond.0,
                                    tgt: 0,
                                });
                                self.compile_expr(Some(dst), expr).eval(self);
                                self.builder
                                    .patch_jump(before, self.builder.next_instruction_id());
                            }
                            tgt.compile_assign(self, dst);
                            tgt.free_temp(self);
                            self.builder.free_temp(dst);
                        }
                        ast::BindingProperty::Ident(atom, ref binding) => {
                            let key = self.compile_atom(None, atom);
                            self.compile_binding_element(binding, key, obj)
                        }
                        ast::BindingProperty::Computed(ref key, ref binding) => {
                            let key = self.compile_expr(None, key).eval(self);
                            self.compile_binding_element(binding, key, obj);
                        }
                    }
                }

                if let Some(_rem) = rem {
                    todo!()
                }
            }
        }
    }

    pub fn compile_binding_element(
        &mut self,
        binding: &'a BindingElement<A>,
        key: Register,
        obj: Register,
    ) {
        match *binding {
            ast::BindingElement::Single(sym, ref expr) => {
                let tgt = AssignmentTarget::Variable(sym);
                // If you have more than 2^31 bindings in code then you deserve a
                // panic.
                let dst = tgt
                    .placement(self)
                    .unwrap_or_else(|| self.builder.alloc_temp());
                self.builder.free_temp(key);
                self.builder.push(Instruction::Index {
                    dst: dst.0,
                    obj: obj.0,
                    key: key.0,
                });
                if let Some(x) = expr {
                    let cond = self.builder.alloc_temp();
                    self.builder.free_temp(cond);
                    self.builder.push(Instruction::IsUndefined {
                        dst: cond.0,
                        op: dst.0,
                    });
                    let before = self.builder.push(Instruction::JumpFalse {
                        cond: cond.0,
                        tgt: 0,
                    });
                    self.compile_expr(Some(dst), x).eval(self);
                    self.builder
                        .patch_jump(before, self.builder.next_instruction_id());
                }
                tgt.compile_assign(self, dst);
                tgt.free_temp(self);
                self.builder.free_temp(dst);
            }
            ast::BindingElement::Binding(ref binding, ref expr) => {
                let new_obj = self.builder.alloc_temp();
                self.builder.free_temp(key);
                self.builder.push(Instruction::Index {
                    dst: new_obj.0,
                    obj: obj.0,
                    key: key.0,
                });
                if let Some(expr) = expr {
                    let cond = self.builder.alloc_temp();
                    self.builder.free_temp(cond);
                    self.builder.push(Instruction::IsUndefined {
                        dst: cond.0,
                        op: new_obj.0,
                    });
                    let before = self.builder.push(Instruction::JumpFalse {
                        cond: cond.0,
                        tgt: 0,
                    });
                    self.compile_expr(Some(new_obj), expr).eval(self);
                    self.builder
                        .patch_jump(before, self.builder.next_instruction_id());
                }
                self.compile_binding(binding, new_obj);
                self.builder.free_temp(new_obj);
            }
        }
    }

    pub fn compile_arrow_function_decl(
        &mut self,
        scope: ScopeId,
        params: &'a Params<A>,
        body: &'a ArrowBody<A>,
    ) -> FunctionId {
        let id = self.builder.push_function(scope);
        let bindings: Vec<_> = params
            .0
            .iter()
            .enumerate()
            .filter_map(|(idx, p)| {
                if idx >= 16 {
                    todo!("more then 16 arguments")
                }
                match p {
                    ast::SymbolOrBinding::Single(x) => {
                        self.builder.registers().alloc_arg(Some(*x));
                        None
                    }
                    ast::SymbolOrBinding::Binding(ref b) => {
                        let reg = self.builder.registers().alloc_arg(None);
                        Some((reg, b))
                    }
                }
            })
            .collect();

        for (reg, b) in bindings {
            self.compile_binding(b, reg);
        }

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
        decl: &'a Option<CForDecl<A>>,
        cond: Option<&'a [Expr<A>]>,
        post: Option<&'a [Expr<A>]>,
        block: &'a Stmt<A>,
    ) {
        if let Some(decl) = decl {
            match decl {
                CForDecl::Expr(x) => {
                    let res = self.compile_expressions(None, x).eval(self);
                    self.builder.free_temp(res);
                }
                CForDecl::Define(x) => {
                    for d in x.iter() {
                        self.compile_define(d);
                    }
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

    pub fn compile_for_in(
        &mut self,
        decl: &'a ForInOfDecl<A>,
        expr: &'a [Expr<A>],
        stmt: &'a Stmt<A>,
    ) {
        let res = self.compile_expressions(None, expr).eval(self);
        self.builder.push(Instruction::IterHead {
            dst: res.0,
            obj: res.0,
        });
        let iter = res;
        let jump_head = self.builder.push(Instruction::Jump { tgt: 0 });

        let (start, jump_iter) = match *decl {
            ForInOfDecl::Define(SymbolOrBinding::Single(x)) => {
                let tgt = AssignmentTarget::Variable(x);
                let dst = tgt
                    .placement(self)
                    .unwrap_or_else(|| self.builder.alloc_temp());
                let start = self.builder.push(Instruction::Iter {
                    dst: dst.0,
                    obj: iter.0,
                });
                let jump_iter = self.builder.push(Instruction::Jump { tgt: 0 });
                tgt.compile_assign(self, dst);
                self.builder.free_temp(dst);
                (start, jump_iter)
            }
            ForInOfDecl::Define(SymbolOrBinding::Binding(ref x)) => {
                let dst = self.builder.alloc_temp();
                let start = self.builder.push(Instruction::Iter {
                    dst: dst.0,
                    obj: iter.0,
                });
                let jump_iter = self.builder.push(Instruction::Jump { tgt: 0 });
                self.compile_binding(x, dst);
                self.builder.free_temp(dst);
                (start, jump_iter)
            }
            ForInOfDecl::Expr(ref x) => match x {
                Expr::Prime(ast::PrimeExpr::Object(_)) => todo!(),
                Expr::Prime(ast::PrimeExpr::Array(_)) => todo!(),
                x => {
                    let dst = self.builder.alloc_temp();
                    let start = self.builder.push(Instruction::Iter {
                        dst: dst.0,
                        obj: iter.0,
                    });
                    let jump_iter = self.builder.push(Instruction::Jump { tgt: 0 });
                    let tgt = AssignmentTarget::from_expr(self, &x);
                    tgt.compile_assign(self, dst);
                    self.builder.free_temp(dst);
                    tgt.free_temp(self);
                    (start, jump_iter)
                }
            },
        };

        self.builder.push_flow_scope();
        self.compile_stmt(stmt);
        let flow_scope = self.builder.pop_flow_scope();

        for x in flow_scope.patch_continue {
            self.builder.patch_jump(x, start);
        }

        let j = self.builder.push(Instruction::Jump { tgt: 0 });
        self.builder.patch_jump(j, start);

        self.builder.free_temp(iter);

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
