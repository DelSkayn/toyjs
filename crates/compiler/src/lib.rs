#![allow(dead_code, unused_imports)]

use bumpalo::{collections::Vec, Bump};

use ast::{Scope, Script, VariableId, Variables};
use common::{collections::HashMap, interner::Interner};
use runtime::bytecode::Bytecode;

mod ssa;
use ssa::{Ssa, SsaId, SsaVec};

mod constants;
use constants::Constants;

mod generate;
use generate::Generator;

mod compile;

pub struct StackInfo {
    pub variable_slots: HashMap<VariableId, u32>,
}

impl StackInfo {
    pub fn script(scope: &Scope, variables: &Variables) -> Self {
        let mut count = 0;
        let mut slots = HashMap::default();
        scope.map_variables(
            |v| {
                if variables[v].kind.is_local() {
                    slots.insert(v, count);
                    count += 1;
                }
            },
            |s| !s.is_function,
        );
        StackInfo {
            variable_slots: slots,
        }
    }
}

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
    stack_info: Option<StackInfo>,
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
            stack_info: None,
            constants: Constants::new(),
            alloc,
            variables,
        }
    }

    pub fn compile_script(mut self, script: Script<'alloc>) -> Bytecode {
        let mut last_stmt = None;
        for stmt in script.1.iter() {
            last_stmt = self.compile_stmt(stmt);
        }
        self.stack_info = Some(StackInfo::script(script.0, self.variables));
        self.ssa.push(Ssa::Return { expr: last_stmt });
        let generator = Generator::new(
            self.alloc,
            self.stack_info.take().unwrap(),
            &self.ssa,
            &self.constants,
            self.interner,
        );
        generator.generate()
    }
}
