#![allow(dead_code, unused_imports)]
#![feature(allocator_api)]

use ast::{Params, ScopeId, Script, Stmt, SymbolId, SymbolTable};
use common::{
    interner::Interner,
    newtype_key,
    slotmap::{SlotKey, SlotStack},
};
use constants::Constants;
use lexical_info::LexicalInfo;
use vm::{
    gc::GcArena,
    instructions::{ByteCode, ByteFunction, Instruction},
};

use std::{
    alloc::{Allocator, Global},
    convert::TryInto,
};

mod expr;
mod register;
use register::Registers;
mod constants;
mod lexical_info;
mod stmt;

newtype_key! {
    pub struct InstructionId(u32);
}

impl InstructionId {
    pub fn requires_long(self) -> bool {
        self.0 as u16 as u32 == self.0
    }
}

newtype_key! {
    pub struct FunctionId(u32);
}

impl FunctionId {
    pub fn requires_long(self) -> bool {
        self.0 as u16 as u32 != self.0
    }
}

pub struct PendingFunction<'a, A: Allocator> {
    id: FunctionId,
    scope: ScopeId,
    args: &'a Params<A>,
    stmts: &'a Vec<Stmt<A>, A>,
}

pub struct Compiler<'a, A: Allocator> {
    symbol_table: &'a SymbolTable<A>,
    instructions: SlotStack<Instruction, InstructionId, A>,
    registers: Registers,
    functions: Vec<ByteFunction>,
    pending_functions: Vec<PendingFunction<'a, A>, A>,
    next_function_id: u32,
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
            pending_functions: Vec::new_in(alloc.clone()),
            next_function_id: 1,
            constants: Constants::new_in(interner, gc, Global),
            alloc,
        }
    }

    pub fn compile_script(
        script: &'a Script<A>,
        symbol_table: &'a SymbolTable<A>,
        interner: &'a Interner,
        gc: &'a GcArena,
        alloc: A,
    ) -> ByteCode {
        let mut this = Compiler::new(symbol_table, interner, gc, symbol_table.global(), alloc);

        let res = script
            .0
            .iter()
            .map(|stmt| this.compile_stmt(stmt))
            .last()
            .flatten();
        if let Some(res) = res {
            this.instructions.push(Instruction::Return { ret: res.0 });
        } else {
            this.instructions
                .push(Instruction::ReturnUndefined { _ignore: () });
        }

        let function = &mut this.functions[0];
        function.registers = this.registers.registers_needed();
        //TODO: propagate size errors
        function.size = this.instructions.len().try_into().unwrap();

        while let Some(x) = this.pending_functions.pop() {
            this.compile_function(x);
        }

        let constants = this.constants.into_constants();
        let instructions = this
            .instructions
            .into_vec()
            .into_iter()
            .collect::<Vec<_>>()
            .into_boxed_slice();

        ByteCode {
            constants,
            functions: this.functions.into_boxed_slice(),
            instructions,
        }
    }

    fn compile_function(&mut self, func: PendingFunction<'a, A>) {
        self.registers.clear();
        //TODO: propagate size errors
        self.functions[func.id.0 as usize].offset = self.instructions.len().try_into().unwrap();

        self.compile_params(func.args);

        func.stmts.iter().for_each(|stmt| {
            self.compile_stmt(stmt);
        });
        match self.instructions.last() {
            Some(Instruction::Return { .. }) | Some(Instruction::ReturnUndefined { .. }) => {}
            _ => {
                self.instructions
                    .push(Instruction::ReturnUndefined { _ignore: () });
            }
        }
        self.functions[func.id.0 as usize].registers = self.registers.registers_needed();
        //TODO: propagate size errors
        self.functions[func.id.0 as usize].size = (self.instructions.len()
            - self.functions[func.id.0 as usize].offset as usize)
            .try_into()
            .unwrap();
    }

    fn push_pending_function(
        &mut self,
        scope: ScopeId,
        args: &'a Params<A>,
        stmts: &'a Vec<Stmt<A>, A>,
    ) -> FunctionId {
        let id = FunctionId(self.next_function_id);
        self.next_function_id = self.next_function_id.checked_add(1).unwrap();
        self.functions.push(ByteFunction {
            offset: 0,
            size: 0,
            registers: 0,
        });
        self.pending_functions.push(PendingFunction {
            id,
            scope,
            args,
            stmts,
        });
        id
    }

    fn patch_jump(&mut self, id: InstructionId, to: InstructionId) {
        let jump = to.0 as i64 - id.0 as i64;
        match self.instructions[id] {
            Instruction::JumpTrue { ref mut tgt, .. } => {
                let x = jump.try_into().unwrap();
                *tgt = x;
            }
            Instruction::JumpFalse { ref mut tgt, .. } => {
                let x = jump.try_into().unwrap();
                *tgt = x;
            }
            Instruction::Jump { ref mut tgt, .. } => {
                let x = jump.try_into().unwrap();
                *tgt = x;
            }
            _ => panic!("instruction is not a patchable jump"),
        }
    }

    fn next_instruction_id(&self) -> InstructionId {
        InstructionId::new(self.instructions.len())
    }
}
