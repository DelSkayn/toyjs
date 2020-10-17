#![allow(dead_code, unused_imports)]

use bumpalo::{collections::Vec, Bump};

use ast::{Scope, Script, VariableId, Variables};
use common::{collections::HashMap, interner::Interner};
use runtime::bytecode::Bytecode;
use std::convert::TryFrom;

mod ssa;
use ssa::{Ssa, SsaId, SsaVec};

mod constants;
use constants::Constants;

mod generate;
use generate::Generator;

mod compile;

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
    scope: Option<&'alloc Scope<'alloc>>,
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
            scope: None,
        }
    }

    pub fn compile_script(mut self, script: Script<'alloc>) -> Bytecode {
        self.generate_header(script.0);
        let slots = script.0.next_slot.get();
        self.scope = Some(script.0);
        let mut last_stmt = None;
        for stmt in script.1.iter() {
            last_stmt = self.compile_stmt(stmt);
        }
        self.ssa.push(Ssa::Return { expr: last_stmt });

        let generator =
            Generator::new(self.alloc, &self.ssa, &self.constants, self.interner, slots);
        generator.generate()
    }

    pub fn generate_header(&mut self, scope: &Scope<'alloc>) {
        let depth = scope.stack_depth;
        let ssa = &mut self.ssa;
        let variables = self.variables;
        for v in scope.variables.borrow().iter().copied() {
            dbg!(v);
            if dbg!(&variables[v]).kind.is_local() {
                dbg!(v);
                ssa.push_env(0);
                break;
            }
        }
        scope.captures.borrow().iter().copied().for_each(|v| {
            let define_depth = variables[v].define_depth;
            let depth = depth - define_depth;
            ssa.push_env(depth);
        });
        scope.traverse_childeren(&mut |s| {
            if s.is_function {
                return false;
            }
            s.captures.borrow().iter().copied().for_each(|v| {
                let define_depth = variables[v].define_depth;
                let depth = depth - define_depth;
                ssa.push_env(depth);
            });
            true
        });

        self.ssa.push_global();
    }
}
