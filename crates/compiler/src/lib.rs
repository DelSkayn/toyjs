#![allow(dead_code, unused_imports)]
#![feature(allocator_api)]

use ast::{ScopeId, Script, SymbolTable};
use common::{
    interner::Interner,
    newtype_key,
    slotmap::{SlotKey, SlotStack},
};
use constants::Constants;
use lexical_info::LexicalInfo;
use runtime::{
    gc::GcArena,
    instructions::{Instruction, InstructionBuffer},
    ByteCode, ByteFunction,
};

use std::alloc::{Allocator, Global};

mod expr;
mod register;
use register::Registers;
mod constants;
mod lexical_info;
mod stmt;

newtype_key! {
    pub struct InstructionId(u32);
}

pub struct Compiler<'a, A: Allocator> {
    symbol_table: &'a SymbolTable<A>,
    instructions: SlotStack<Instruction, InstructionId, A>,
    registers: Registers,
    functions: Vec<ByteFunction>,
    constants: Constants<'a, Global>,
    lexical_info: LexicalInfo<A>,
    alloc: A,
}

impl<'a, A: Allocator + Clone> Compiler<'a, A> {
    fn new(
        symbol_table: &'a SymbolTable<A>,
        interner: &'a Interner,
        gc: &'a GcArena,
        root: ScopeId,
        alloc: A,
    ) -> Self {
        Compiler {
            lexical_info: LexicalInfo::new_in(root, &symbol_table, alloc.clone()),
            symbol_table,
            instructions: SlotStack::new_in(alloc.clone()),
            registers: Registers::new(),
            functions: vec![ByteFunction {
                offset: 0,
                size: 0,
                registers: 0,
            }],
            constants: Constants::new_in(interner, gc, Global),
            alloc,
        }
    }

    pub fn compile_script(
        script: &Script<A>,
        symbol_table: &'a SymbolTable<A>,
        interner: &'a Interner,
        gc: &'a GcArena,
        alloc: A,
    ) -> ByteCode {
        let mut this = Compiler::new(symbol_table, interner, gc, symbol_table.global(), alloc);

        let mut res = None;
        for stmt in script.0.iter() {
            res = this.compile_stmt(stmt);
        }
        if let Some(res) = res {
            this.instructions.push(Instruction::Return {
                ret: res.0,
                null: 0,
            });
        } else {
            this.instructions
                .push(Instruction::ReturnUndefined { nul0: 0, nul1: 0 });
        }

        let constants = this.constants.into_constants();

        let function = this.functions.last_mut().unwrap();
        function.registers = this.registers.registers_needed();
        function.size = this.instructions.len();

        let instructions = InstructionBuffer::from_instructions(&this.instructions);

        ByteCode {
            constants,
            functions: this.functions.into_boxed_slice(),
            instructions,
        }
    }
}
