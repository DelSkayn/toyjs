#![allow(dead_code, unused_imports)]
#![feature(allocator_api)]

use ast::{Script, SymbolTable};
use common::{
    interner::Interner,
    newtype_key,
    slotmap::{SlotKey, SlotStack},
};
use constants::Constants;
use runtime::{
    gc::GcArena,
    instructions::{Instruction, InstructionBuffer},
    ByteCode,
};

use std::alloc::{Allocator, Global};

mod expr;
mod register;
use register::Registers;
mod constants;
mod stmt;

newtype_key! {
    pub struct InstructionId(u32);
}

pub struct Compiler<'a, A: Allocator> {
    symbol_table: &'a SymbolTable<A>,
    instructions: SlotStack<Instruction, InstructionId, A>,
    registers: Registers,
    constants: Constants<'a, Global>,
    alloc: A,
}

impl<'a, A: Allocator + Clone> Compiler<'a, A> {
    pub fn new(
        symbol_table: &'a SymbolTable<A>,
        interner: &'a Interner,
        gc: &'a GcArena,
        alloc: A,
    ) -> Self {
        Compiler {
            symbol_table,
            instructions: SlotStack::new_in(alloc.clone()),
            registers: Registers::new(),
            constants: Constants::new_in(interner, gc, Global),
            alloc,
        }
    }

    pub fn compile_script(mut self, script: &Script<A>) -> ByteCode {
        let mut res = None;
        for stmt in script.0.iter() {
            res = self.compile_stmt(stmt);
        }
        if let Some(res) = res {
            self.instructions.push(Instruction::Return {
                ret: res.0,
                null: 0,
            });
        } else {
            self.instructions
                .push(Instruction::ReturnUndefined { nul0: 0, nul1: 0 });
        }

        let constants = self.constants.into_constants();
        let instructions = InstructionBuffer::from_instructions(&self.instructions);

        ByteCode {
            constants,
            instructions,
        }
    }
}
