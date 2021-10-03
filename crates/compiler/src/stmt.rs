use ast::Stmt;

use crate::{register::Register, Compiler};
use std::alloc::Allocator;

impl<'a, A: Allocator + Clone> Compiler<'a, A> {
    pub(crate) fn compile_stmt(&mut self, stmt: &Stmt<A>) -> Option<Register> {
        match stmt {
            Stmt::Expr(x) => Some(
                x.iter()
                    .map(|x| {
                        let place = self.compile_expr(None, x);
                        self.registers.free_temp(place.place);
                        place.place
                    })
                    .last()
                    .expect("expression node did not have atleast a single expression"),
            ),
            Stmt::Let(symbol, expr) => {
                let reg = self.registers.alloc_symbol(*symbol);
                //TODO captured variables
                expr.as_ref().map(|x| self.compile_expr(Some(reg), x).place)
            }
            Stmt::Const(symbol, expr) => {
                let reg = self.registers.alloc_symbol(*symbol);
                //TODO captured variables
                Some(self.compile_expr(Some(reg), expr).place)
            }
            _ => todo!(),
        }
    }
}
