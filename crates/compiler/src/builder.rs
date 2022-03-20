use std::{alloc::Allocator, convert::TryInto};

use ast::{ScopeId, SymbolId};
use common::{
    newtype_key,
    slotmap::{SlotKey, SlotStack},
};
use vm::instructions::{ByteFunction, Instruction, Upvalue as BcUpvalue};

use crate::register::{Register, Registers};

newtype_key! {
    pub struct InstructionId(u32);
}

newtype_key! {
    pub struct FunctionId(pub (crate) u16);
}

newtype_key! {
    pub struct UpvalueSlot(pub(crate) u16);
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Upvalue {
    kind: UpvalueKind,
    symbol: SymbolId,
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum UpvalueKind {
    /// An upvalue is local if the scope that created the function object contains the value.
    Local(Register),
    Parent(UpvalueSlot),
}

pub struct LoopFlowScope<A: Allocator> {
    /// The first instruction of the loop
    pub patch_break: Vec<InstructionId, A>,
    /// jumps which need to be patched to the end of the loop.
    pub patch_continue: Vec<InstructionId, A>,
}

struct BuilderFunction<A: Allocator + Clone> {
    parent: Option<FunctionId>,
    scope: ScopeId,
    instructions: SlotStack<Instruction, InstructionId, A>,
    upvalues: SlotStack<Upvalue, UpvalueSlot, A>,
    registers: Registers,
    flow_scope: Vec<LoopFlowScope<A>, A>,
}

impl<A: Allocator + Clone> BuilderFunction<A> {
    fn find_upvalue(&self, symbol: SymbolId) -> Option<UpvalueSlot> {
        self.upvalues.iter().enumerate().find_map(|(idx, x)| {
            if x.symbol == symbol {
                Some(UpvalueSlot(idx as u16))
            } else {
                None
            }
        })
    }
}

pub struct ScriptBuilder<A: Allocator + Clone> {
    functions: SlotStack<BuilderFunction<A>, FunctionId, A>,
    current: FunctionId,
    alloc: A,
}

impl<A: Allocator + Clone> ScriptBuilder<A> {
    pub fn new_in(alloc: A, start_scope: ScopeId) -> Self {
        let mut functions = SlotStack::new_in(alloc.clone());
        let current = functions.push(BuilderFunction {
            parent: None,
            scope: start_scope,
            instructions: SlotStack::new_in(alloc.clone()),
            registers: Registers::new(),
            upvalues: SlotStack::new_in(alloc.clone()),
            flow_scope: Vec::new_in(alloc.clone()),
        });

        ScriptBuilder {
            functions,
            current,
            alloc,
        }
    }

    pub fn push_function(&mut self, scope: ScopeId, params: &ast::Params<A>) -> FunctionId {
        let res = self.functions.len().try_into().expect("to many functions");
        let mut registers = Registers::new();

        for (idx, p) in params.0.iter().enumerate() {
            if idx >= 16 {
                todo!()
            }
            registers.alloc_arg(*p);
        }

        self.functions.push(BuilderFunction {
            parent: Some(self.current),
            scope,
            instructions: SlotStack::new_in(self.alloc.clone()),
            registers,
            upvalues: SlotStack::new_in(self.alloc.clone()),
            flow_scope: Vec::new_in(self.alloc.clone()),
        });
        self.current = FunctionId(res);
        self.current
    }

    pub fn pop_function(&mut self) {
        self.current = self.functions[self.current]
            .parent
            .expect("tried to pop parent function");
    }

    pub fn push(&mut self, instr: Instruction) -> InstructionId {
        self.functions[self.current].instructions.push(instr)
    }

    pub fn push_flow_scope(&mut self) {
        self.functions[self.current].flow_scope.push(LoopFlowScope {
            patch_break: Vec::new_in(self.alloc.clone()),
            patch_continue: Vec::new_in(self.alloc.clone()),
        });
    }

    pub fn push_break(&mut self) {
        let id = self.push(Instruction::Jump { tgt: 0 });
        self.functions[self.current]
            .flow_scope
            .last_mut()
            .expect("no active control flow scope")
            .patch_break
            .push(id);
    }

    pub fn push_continue(&mut self) {
        let id = self.push(Instruction::Jump { tgt: 0 });
        self.functions[self.current]
            .flow_scope
            .last_mut()
            .expect("no active control flow scope")
            .patch_continue
            .push(id);
    }

    pub fn pop_flow_scope(&mut self) -> LoopFlowScope<A> {
        self.functions[self.current]
            .flow_scope
            .pop()
            .expect("no active control flow scope")
    }

    pub fn lexical_scope(&self) -> ScopeId {
        self.functions[self.current].scope
    }

    pub fn patch_jump(&mut self, id: InstructionId, to: InstructionId) {
        let jump = to.0 as i64 - id.0 as i64;
        let x = jump.try_into().unwrap();
        match self.functions[self.current].instructions[id] {
            Instruction::JumpTrue { ref mut tgt, .. } => {
                *tgt = x;
            }
            Instruction::JumpFalse { ref mut tgt, .. } => {
                *tgt = x;
            }
            Instruction::Jump { ref mut tgt, .. } => {
                *tgt = x;
            }
            Instruction::Try { ref mut tgt, .. } => {
                *tgt = x;
            }
            _ => panic!("instruction is not a patchable jump"),
        }
    }

    pub fn capture_upvalue(&mut self, symbol: SymbolId, var_scope: ScopeId) -> UpvalueSlot {
        if let Some(x) = self.functions[self.current].find_upvalue(symbol) {
            return x;
        }

        let mut parent = self.functions[self.current]
            .parent
            .expect("tried to capture a variable of a non parent scope");

        // Is the upvalue local
        if self.functions[parent].scope == var_scope {
            let register = self.functions[parent].registers.alloc_symbol(symbol);
            return self.functions[self.current].upvalues.push(Upvalue {
                kind: UpvalueKind::Local(register),
                symbol,
            });
        }

        std::mem::swap(&mut self.current, &mut parent);
        let slot = self.capture_upvalue(symbol, var_scope);
        std::mem::swap(&mut self.current, &mut parent);
        self.functions[self.current].upvalues.push(Upvalue {
            kind: UpvalueKind::Parent(slot),
            symbol,
        })
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
        &mut self.functions[self.current].registers
    }

    pub fn instructions(&self) -> &SlotStack<Instruction, InstructionId, A> {
        &self.functions[self.current].instructions
    }

    pub fn instructions_mut(&mut self) -> &mut SlotStack<Instruction, InstructionId, A> {
        &mut self.functions[self.current].instructions
    }

    pub fn build(self) -> (Box<[ByteFunction]>, Box<[Instruction]>) {
        let mut instructions = Vec::new();
        let mut functions = Vec::new();
        for f in self.functions.into_vec() {
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

            let upvalues = f
                .upvalues
                .iter()
                .map(|x| match x.kind {
                    UpvalueKind::Local(x) => BcUpvalue::Local(x.0),
                    UpvalueKind::Parent(x) => BcUpvalue::Parent(x.0),
                })
                .collect::<Vec<_>>()
                .into_boxed_slice();

            functions.push(ByteFunction {
                registers,
                size,
                offset,
                upvalues,
            });
            instructions.extend_from_slice(&func_instructions);
        }
        (
            functions.into_boxed_slice(),
            instructions.into_boxed_slice(),
        )
    }
}
