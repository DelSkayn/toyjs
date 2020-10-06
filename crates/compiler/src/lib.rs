#![allow(dead_code, unused_imports)]

use ast::{Script, Variables};
use common::interner::Interner;
use runtime::bytecode::Bytecode;

mod ssa;
use ssa::{Ssa, SsaId, SsaVec};

mod constants;
use constants::Constants;

mod generate;
use generate::Generator;

mod compile;

use bumpalo::{collections::Vec, Bump};
struct ExprRes<'alloc> {
    pub value: SsaId,
    pub true_list: Vec<'alloc, SsaId>,
    pub false_list: Vec<'alloc, SsaId>,
}

impl<'alloc> ExprRes<'alloc> {
    pub fn new(value: SsaId, alloc: &'alloc Bump) -> Self {
        ExprRes {
            value,
            true_list: Vec::new_in(alloc),
            false_list: Vec::new_in(alloc),
        }
    }
}

pub struct Compiler<'a, 'alloc> {
    alloc: &'alloc Bump,
    interner: &'a Interner,
    ssa: SsaVec<'alloc>,
    constants: Constants,
    variables: &'a Variables<'alloc>,
}

impl<'a, 'alloc> Compiler<'a, 'alloc> {
    pub fn new(
        alloc: &'alloc Bump,
        interner: &'a Interner,
        variables: &'a Variables<'alloc>,
    ) -> Self {
        Compiler {
            ssa: SsaVec::new_in(alloc),
            interner,
            constants: Constants::new(),
            alloc,
            variables,
        }
    }

    pub fn compile_script(mut self, script: Script<'alloc>) -> Bytecode {
        let mut last_stmt = None;
        for stmt in script.0.iter() {
            last_stmt = self.compile_stmt(stmt);
        }
        self.ssa.push(Ssa::Return { expr: last_stmt });
        let generator = Generator::new(self.alloc, &self.ssa, &self.constants, self.interner);
        generator.generate()
    }
}
