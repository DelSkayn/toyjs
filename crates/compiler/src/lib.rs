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
    gc::GcArena,
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

mod expr;
mod stmt;

newtype_key! {
    pub struct InstructionId(u32);
}

newtype_key! {
    pub struct FunctionId(u16);
}

struct BuilderFunction<A: Allocator + Clone> {
    parent: Option<FunctionId>,
    scope: ScopeId,
    instructions: SlotStack<Instruction, InstructionId, A>,
    registers: Registers,
}

pub struct ScriptBuilder<A: Allocator + Clone> {
    functions: Vec<BuilderFunction<A>, A>,
    current_function: FunctionId,
    alloc: A,
}

impl<A: Allocator + Clone> ScriptBuilder<A> {
    pub fn new_in(alloc: A, start_scope: ScopeId) -> Self {
        let mut functions = Vec::with_capacity_in(1, alloc.clone());
        functions.push(BuilderFunction {
            parent: None,
            scope: start_scope,
            instructions: SlotStack::new_in(alloc.clone()),
            registers: Registers::new(),
        });

        ScriptBuilder {
            functions,
            current_function: FunctionId(0),
            alloc,
        }
    }

    pub fn push_function(&mut self, scope: ScopeId, params: &Params<A>) -> FunctionId {
        let res = self.functions.len().try_into().expect("to many functions");
        let mut registers = Registers::new();

        for (idx, p) in params.0.iter().enumerate() {
            if idx >= 16 {
                todo!()
            }
            registers.alloc_arg(*p);
        }

        self.functions.push(BuilderFunction {
            parent: Some(self.current_function),
            scope,
            instructions: SlotStack::new_in(self.alloc.clone()),
            registers,
        });
        self.current_function = FunctionId(res);
        self.current_function
    }

    pub fn pop_function(&mut self) {
        self.current_function = self.functions[self.current_function.0 as usize]
            .parent
            .expect("tried to pop parent function");
    }

    pub fn push(&mut self, instr: Instruction) -> InstructionId {
        self.functions[self.current_function.0 as usize]
            .instructions
            .push(instr)
    }

    pub fn scope(&self) -> ScopeId {
        self.functions[self.current_function.0 as usize].scope
    }

    pub fn patch_jump(&mut self, id: InstructionId, to: InstructionId) {
        let jump = to.0 as i64 - id.0 as i64;
        let x = jump.try_into().unwrap();
        match self.functions[self.current_function.0 as usize].instructions[id] {
            Instruction::JumpTrue { ref mut tgt, .. } => {
                *tgt = x;
            }
            Instruction::JumpFalse { ref mut tgt, .. } => {
                *tgt = x;
            }
            Instruction::Jump { ref mut tgt, .. } => {
                *tgt = x;
            }
            _ => panic!("instruction is not a patchable jump"),
        }
    }

    pub fn next_instruction_id(&self) -> InstructionId {
        InstructionId::new(self.instructions().len())
    }

    pub fn free_temp(&mut self, register: Register) {
        self.registers().free_temp(register);
    }

    pub fn alloc_temp(&mut self) -> Register {
        self.registers().alloc_temp()
    }

    pub fn alloc_symbol(&mut self, symbol: SymbolId) -> Register {
        self.registers().alloc_symbol(symbol)
    }

    pub fn registers(&mut self) -> &mut Registers {
        &mut self.functions[self.current_function.0 as usize].registers
    }

    pub fn instructions(&self) -> &SlotStack<Instruction, InstructionId, A> {
        &self.functions[self.current_function.0 as usize].instructions
    }

    pub fn instructions_mut(&mut self) -> &mut SlotStack<Instruction, InstructionId, A> {
        &mut self.functions[self.current_function.0 as usize].instructions
    }

    pub fn build(self) -> (Box<[ByteFunction]>, Box<[Instruction]>) {
        let mut instructions = Vec::new();
        let mut functions = Vec::new();
        for f in self.functions {
            let registers = f.registers.registers_needed();
            let func_instructions = f.instructions.into_vec();
            let size = func_instructions
                .len()
                .try_into()
                .expect("too many instructions");
            let offset = instructions
                .len()
                .try_into()
                .expect("too many instructions");
            functions.push(ByteFunction {
                registers,
                size,
                offset,
            });
            instructions.extend_from_slice(&func_instructions);
        }
        (
            functions.into_boxed_slice(),
            instructions.into_boxed_slice(),
        )
    }
}

pub struct Compiler<'a, A: Allocator + Clone> {
    alloc: A,
    symbol_table: &'a SymbolTable<A>,
    constants: Constants<'a, Global>,
    builder: ScriptBuilder<A>,
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
            symbol_table,
            constants: Constants::new_in(interner, gc, Global),
            builder: ScriptBuilder::new_in(alloc.clone(), root),
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
            this.builder.push(Instruction::Return { ret: res.0 });
        } else {
            this.builder
                .push(Instruction::ReturnUndefined { _ignore: () });
        }

        let constants = this.constants.into_constants();
        let (functions, instructions) = this.builder.build();

        ByteCode {
            constants,
            instructions,
            functions,
        }
    }

    fn compile_function(&mut self) {
        todo!()
    }
}
