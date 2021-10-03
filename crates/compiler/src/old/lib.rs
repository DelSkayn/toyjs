#![allow(dead_code, unused_imports)]
#![feature(allocator_api)]

use ast::{ScopeId, Script, SymbolId, SymbolTable};

use common::{collections::HashMap, interner::Interner};
use runtime::bytecode::Module;
use std::{alloc::Allocator, convert::TryFrom, mem, ptr};

mod ssa;
use ssa::{Ssa, SsaId, SsaVec};

mod constants;
use constants::Constants;

mod generate;
use generate::{Generator, ModuleBuilder};

mod compile;

struct ExprRes<A: Allocator> {
    pub value: SsaId,
    pub true_list: Vec<SsaId, A>,
    pub false_list: Vec<SsaId, A>,
}

impl<A: Allocator + Clone> ExprRes<A> {
    pub fn new_in(value: SsaId, alloc: A) -> Self {
        ExprRes {
            value,
            true_list: Vec::new_in(alloc.clone()),
            false_list: Vec::new_in(alloc),
        }
    }
}

/// The compiler, takes an abstract syntax tree and produces bytecode.
pub struct Compiler<'a, A: Allocator> {
    interner: &'a Interner,
    ssa: SsaVec,
    module: ModuleBuilder,
    variables: &'a SymbolTable<A>,
    scope: Option<SymbolId>,
    dump_ssa: bool,
    dump_bc: bool,
    alloc: A,
}

impl<'a, A: Allocator> Compiler<'a, A> {
    pub fn new_in(interner: &'a Interner, variables: &'a SymbolTable<A>, alloc: A) -> Self {
        Compiler {
            ssa: SsaVec::new(),
            module: ModuleBuilder::new(),
            interner,
            alloc,
            variables,
            scope: None,
            dump_ssa: false,
            dump_bc: false,
        }
    }

    pub fn dump_ssa(mut self, dump: bool) -> Self {
        self.dump_ssa = dump;
        self
    }

    pub fn dump_bc(mut self, dump: bool) -> Self {
        self.dump_bc = dump;
        self
    }

    pub fn compile_script(mut self, script: Script<A>) -> Module {
        self.generate_header(script.0);
        let slots = script.0.ty.as_function().next_slot.get();
        self.scope = Some(script.0);
        let mut last_stmt = None;
        for stmt in script.1.iter() {
            last_stmt = self.compile_stmt(stmt);
        }
        self.ssa.push(Ssa::Return { expr: last_stmt });
        println!("==== SSA ==== ");
        println!("{}", self.ssa);

        let generator = Generator::new(
            "__main__".to_string(),
            &self.ssa,
            self.interner,
            &mut self.module,
            slots,
        );
        generator.generate();

        let module = self.module.build(self.interner);

        if self.dump_bc {
            println!("==== Byte Code ==== ");
            println!("{}", module);
        }
        module
    }

    pub fn generate_header(&mut self, scope: ScopeId) {
        let depth = scope.ty.as_function().stack_depth;
        let ssa = &mut self.ssa;
        let variables = self.variables;
        for v in scope.variables.borrow().iter().copied() {
            if variables[v].kind.is_local() {
                ssa.push_env(0);
                break;
            }
        }
        scope
            .ty
            .as_function()
            .captures
            .borrow()
            .iter()
            .copied()
            .for_each(|v| {
                let define_depth = variables[v].define_depth;
                let depth = depth - define_depth;
                ssa.push_env(depth);
            });
        scope.traverse_childeren(&mut |s| {
            if s.is_function() {
                return false;
            }
            for v in s.variables.borrow().iter().copied() {
                if variables[v].kind.is_local() {
                    ssa.push_env(0);
                    break;
                }
            }
            true
        });

        self.ssa.push_global();
    }

    pub fn compile_function(
        &mut self,
        scope: ScopeId,
        id: SymbolId,
        _args: &ast::Params<A>,
        stmts: &Vec<A, ast::Stmt<A>>,
    ) {
        let old_ssa = mem::replace(&mut self.ssa, SsaVec::new());
        self.generate_header(scope);
        let name = self
            .interner
            .lookup(self.variables[id].name)
            .unwrap()
            .to_string();
        let slots = scope.ty.as_function().next_slot.get();
        for stmt in stmts.iter() {
            self.compile_stmt(stmt);
        }
        match self.ssa[self.ssa.cur()] {
            Ssa::Return { .. } => {}
            _ => {
                self.ssa.insert(Ssa::Return { expr: None });
            }
        };
        println!("==== SSA ==== ");
        println!("{}", self.ssa);

        let generator = Generator::new(name, &self.ssa, self.interner, &mut self.module, slots);
        let function = generator.generate();
        self.ssa = old_ssa;
        if scope.parent.unwrap().parent_function.unwrap() == self.variables.root() {
            let object = self.ssa.global();
            let constant = self.module.constants.add_string(self.variables[id].name);
            let key = self.ssa.insert(Ssa::LoadString { constant });
            let function = self.ssa.insert(Ssa::CreateFunction { function });
            self.ssa.insert(Ssa::Assign {
                object,
                key,
                value: function,
            });
        } else {
            todo!()
        }
    }
}