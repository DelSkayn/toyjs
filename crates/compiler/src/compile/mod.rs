use crate::Compiler;
use crate::{constants::Constant, ssa::*};
use ast::*;
use bumpalo::{collections::Vec, Bump};
use common::interner::StringId;

mod expr;
use expr::Place;

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
                    stmt_expr = Some(self.compile_expr(expr));
                }
            }
            Stmt::Empty => {}
            Stmt::Var(var, ref exprs) => {
                if let Some(x) = exprs.as_ref() {
                    stmt_expr = Some(self.compile_expr(x));
                    self.compile_assignment(Place::Variable(var), stmt_expr.unwrap());
                }
            }
            _ => todo!(),
        }
        stmt_expr
    }
}
