use ast::Stmt;

use crate::Compiler;
use std::alloc::Allocator;

impl<'a, A: Allocator> Compiler<'a, A> {
    pub(crate) fn compile_stmt(&mut self, stmt: &Stmt<A>) {
        match stmt {
            _ => todo!(),
        }
    }
}
