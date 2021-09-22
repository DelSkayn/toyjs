use crate::Compiler;
use crate::{constants::Constant, ssa::*};
use ast::*;
use bumpalo::{collections::Vec, Bump};
use common::interner::StringId;

mod flow;

mod expr;
use expr::{CompiledExpr, Place};

impl<'a, 'alloc> Compiler<'a, 'alloc> {
    pub(crate) fn compile_stmt(&mut self, stmt: &Stmt<'alloc>) -> Option<SsaId> {
        let mut stmt_expr = None;
        match *stmt {
            Stmt::Block(_scope, ref stmts) => {
                for stmt in stmts.iter() {
                    stmt_expr = self.compile_stmt(stmt);
                }
            }
            Stmt::Expr(ref exprs) => {
                for expr in exprs.iter() {
                    stmt_expr = Some(self.compile_expr(expr).eval(self));
                }
            }
            Stmt::Empty => {}
            Stmt::Var(var, ref exprs) => {
                if let Some(x) = exprs.as_ref() {
                    stmt_expr = Some(self.compile_expr(x).eval(self));
                    self.compile_assignment(Place::Variable(var), stmt_expr.unwrap());
                }
            }
            Stmt::Let(var, ref exprs) => {
                if let Some(x) = exprs.as_ref() {
                    let expr = self.compile_expr(x);
                    stmt_expr = Some(expr.eval(self));
                    self.compile_assignment(Place::Variable(var), stmt_expr.unwrap());
                } else {
                    let constant = self.module.constants.add(Constant::Undefined);
                    let expr = self.ssa.insert(Ssa::LoadConstant { constant });
                    self.compile_assignment(Place::Variable(var), expr);
                }
            }
            Stmt::If(ref cond, ref if_stmt, ref else_stmt) => {
                let mut expr: Option<CompiledExpr> = None;
                for e in cond.iter() {
                    if let Some(x) = expr.take() {
                        x.eval(self);
                    }
                    expr = Some(self.compile_expr(e));
                }
                let expr = expr.unwrap();
                let jump = self.ssa.push(Ssa::ConditionalJump {
                    condition: expr.id(),
                    to: None,
                    jump_true: false,
                });
                let next = self.ssa.cur().next();
                for j in expr.true_list.iter().copied() {
                    self.ssa.patch_jump(j, next);
                }
                self.compile_stmt(if_stmt);
                if let Some(x) = else_stmt {
                    let jump_else = self.ssa.push(Ssa::Jump { to: None });
                    self.ssa.patch_jump(jump, jump_else.next());
                    for j in expr.false_list.iter().copied() {
                        self.ssa.patch_jump(j, jump_else.next());
                    }
                    self.compile_stmt(x);
                    self.ssa.patch_jump(jump_else, self.ssa.cur().next());
                } else {
                    let next = self.ssa.cur().next();
                    self.ssa.patch_jump(jump, next);
                    for j in expr.false_list.iter().copied() {
                        self.ssa.patch_jump(j, next);
                    }
                }
            }
            Stmt::While(ref cond, ref stmt) => {
                let before_cond = self.ssa.cur().next();
                let mut expr: Option<CompiledExpr> = None;
                for e in cond.iter() {
                    if let Some(x) = expr.take() {
                        x.eval(self);
                    }
                    expr = Some(self.compile_expr(e));
                }
                let expr = expr.unwrap();
                let jump = self.ssa.push(Ssa::ConditionalJump {
                    condition: expr.id(),
                    jump_true: false,
                    to: None,
                });
                let before_stmt = self.ssa.cur().next();
                for j in expr.true_list.iter().copied() {
                    self.ssa.patch_jump(j, before_stmt);
                }
                self.compile_stmt(stmt);
                self.ssa.push(Ssa::Jump {
                    to: Some(before_cond),
                });
                let after_stmt = self.ssa.cur().next();
                self.ssa.patch_jump(jump, after_stmt);
                for j in expr.false_list.iter().copied() {
                    self.ssa.patch_jump(j, after_stmt);
                }
            }
            Stmt::Function(scope, ref id, ref args, ref stmt) => {
                self.compile_function(scope, *id, args, stmt);
            }
            Stmt::Return(ref exprs) => {
                if let Some(exprs) = exprs {
                    let mut ret_expr = None;
                    for expr in exprs.iter() {
                        ret_expr = Some(self.compile_expr(expr).eval(self));
                    }
                    self.ssa.push(Ssa::Return { expr: ret_expr });
                } else {
                    self.ssa.push(Ssa::Return { expr: None });
                }
            }
            ref x => {
                println!("{:?}", x);
                todo!();
            }
        }
        stmt_expr
    }
}
