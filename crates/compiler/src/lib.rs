#![allow(dead_code, unused_imports)]
#![feature(allocator_api)]

use ast::{Params, ScopeId, Script, Stmt, SymbolId, SymbolTable, SymbolTableBuilder};
use common::{
    interner::Interner,
    newtype_key,
    slotmap::{SlotKey, SlotStack},
};
use dreck::Root;
//use constants::Constants;
//use lexical_info::LexicalInfo;
use vm::{
    atom::Atoms,
    instructions::{ByteCode, ByteFunction, Instruction},
};

use std::{
    alloc::{Allocator, Global},
    convert::TryInto,
};

mod register;
use register::{Register, Registers};
mod constants;
use constants::Constants;
mod builder;
use builder::InstructionId;
use builder::ScriptBuilder;

mod expr;
mod stmt;

pub struct Compiler<'gc, 'own, A: Allocator + Clone> {
    alloc: A,
    constants: Constants<'gc, 'own, Global>,
    builder: ScriptBuilder<'gc, A>,
}

impl<'gc, 'own, A: Allocator + Clone> Compiler<'gc, 'own, A> {
    fn new(
        symbol_table: SymbolTableBuilder<'gc, A>,
        gc: &'gc Root<'own>,
        atoms: &'gc mut Atoms<'gc, 'own>,
        interner: &'gc mut Interner,
        root: ScopeId,
        alloc: A,
    ) -> Self {
        Compiler {
            constants: Constants::new_in(gc, atoms, interner, Global),
            builder: ScriptBuilder::new_in(alloc.clone(), symbol_table, root),
            alloc,
        }
    }

    pub fn compile_script(
        script: &'gc Script<A>,
        symbol_table: SymbolTableBuilder<'gc, A>,
        gc: &'gc Root<'own>,
        atoms: &'gc mut Atoms<'gc, 'own>,
        interner: &'gc mut Interner,
        alloc: A,
    ) -> ByteCode<'gc, 'own> {
        let mut this = Compiler::new(symbol_table, gc, atoms, interner, ScopeId::root(), alloc);

        let res = script
            .0
            .iter()
            .map(|stmt| this.compile_stmt(stmt))
            .last()
            .flatten();

        if let Some(res) = res {
            this.builder.push(Instruction::Return { ret: res.0 });
        } else {
            this.builder
                .push(Instruction::ReturnUndefined { _ignore: () });
        }

        let constants = this.constants.into_constants();
        let (functions, instructions) = this.builder.build();

        ByteCode {
            constants,
            functions,
            instructions,
        }
    }
}
