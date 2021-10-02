use ast::Stmt;

use crate::{register::Register, Compiler};
use std::alloc::Allocator;

impl<'a, A: Allocator + Clone> Compiler<'a, A> {
    pub(crate) fn compile_stmt(&mut self, stmt: &Stmt<A>) -> Option<Register> {
        match stmt {
            Stmt::Expr(x) => Some(
                x.iter()
                    .map(|x| {
                        let place = self.compile_expr(x);
                        self.registers.free_temp(place.place);
                        place.place
                    })
                    .last()
                    .expect("expression node did not have atleast a single expression"),
            ),
            _ => todo!(),
        }
    }
}
