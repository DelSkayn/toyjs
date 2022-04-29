#![allow(dead_code, unused_imports)]
#![feature(allocator_api)]

use ast::{Params, ScopeId, Script, Stmt, SymbolId, SymbolTable};
use common::{
    interner::Interner,
    newtype_key,
    slotmap::{SlotKey, SlotStack},
};
//use constants::Constants;
//use lexical_info::LexicalInfo;
use vm::{
    atom::Atoms,
    gc,
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

pub struct Compiler<'a, 'rt, 'cell, A: Allocator + Clone> {
    alloc: A,
    symbol_table: &'a SymbolTable<A>,
    constants: Constants<'a, 'rt, 'cell, Global>,
    builder: ScriptBuilder<'a, A>,
}

impl<'a, 'rt, 'cell, A: Allocator + Clone> Compiler<'a, 'rt, 'cell, A> {
    fn new(
        symbol_table: &'a SymbolTable<A>,
        interner: &'a mut Interner,
        atoms: &'a Atoms,
        gc: &'a gc::Arena<'rt, 'cell>,
        root: ScopeId,
        alloc: A,
    ) -> Self {
        Compiler {
            symbol_table,
            constants: Constants::new_in(interner, atoms, gc, Global),
            builder: ScriptBuilder::new_in(alloc.clone(), symbol_table, root),
            alloc,
        }
    }

    pub fn compile_script(
        script: &'a Script<A>,
        symbol_table: &'a SymbolTable<A>,
        interner: &'a mut Interner,
        atoms: &'a Atoms,
        gc: &'a gc::Arena<'rt, 'cell>,
        alloc: A,
    ) -> ByteCode<'a, 'cell> {
        let mut this = Compiler::new(
            symbol_table,
            interner,
            atoms,
            gc,
            symbol_table.global(),
            alloc,
        );

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
