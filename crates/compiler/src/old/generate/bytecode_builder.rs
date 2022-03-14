use crate::{
    constants::{ConstStringId, Constant, ConstantId, Constants},
    ssa::SsaId,
};
use bumpalo::{collections::Vec as BumpVec, Bump};
use common::{collections::HashMap, interner::Interner, newtype_vec};
use runtime::{
    bytecode::{op, op_a, op_d, op_op, type_d, Bytecode, Instruction, ModuleFunction, Op},
    value::JSValue,
};
use std::{
    convert::{TryFrom, TryInto},
    rc::Rc,
};

#[derive(Debug)]
pub struct SsaIdToBytecode(Vec<Option<u32>>);
newtype_vec!(struct SsaIdToBytecode[SsaId] -> Option<u32,>);

pub struct BytecodeBuilder<'a> {
    name: String,
    interner: &'a Interner,
    returns: Vec<usize>,
    jumps: Vec<(usize, SsaId)>,
    to_bc: SsaIdToBytecode,
    constants: &'a Constants,
    instructions: &'a mut Vec<Instruction>,
    offset: u32,
}

impl<'a> BytecodeBuilder<'a> {
    pub fn new(
        name: String,
        constants: &'a Constants,
        interner: &'a Interner,
        instructions: &'a mut Vec<Instruction>,
    ) -> Self {
        let to_bc = SsaIdToBytecode(Vec::new());
        instructions.push(0);
        BytecodeBuilder {
            name,
            interner,
            constants,
            offset: u32::try_from(instructions.len() - 1).unwrap(),
            to_bc,
            returns: Vec::new(),
            jumps: Vec::new(),
            instructions,
        }
    }

    pub fn push(&mut self, id: SsaId, instruction: Instruction) {
        while self.to_bc.len() <= id.into() {
            self.to_bc.push(None);
        }
        assert!(self.to_bc[id].is_none());
        self.to_bc[id] = Some(
            self.instructions
                .len()
                .try_into()
                .expect("to many instructions to fit into u32"),
        );
        if op_op(instruction) == op::Return || op_op(instruction) == op::ReturnUndefined {
            self.returns.push(self.instructions.len());
        }
        self.instructions.push(instruction);
    }

    pub fn create_function(&mut self, lifetime: SsaId, dst: u8, function_id: u32) {
        if function_id < u16::MAX as u32 {
            self.push(lifetime, type_d(Op::LoadData, dst, function_id as u16));
        } else {
            self.push(lifetime, type_d(Op::LoadData, dst, u16::MAX));
            self.instructions.push(function_id);
        }
    }

    pub fn constant(&mut self, lifetime: SsaId, dst: u8, constant: ConstantId) {
        let id: u32 = constant.into();
        if id < u16::MAX as u32 {
            self.push(lifetime, type_d(Op::LoadData, dst, id as u16));
        } else {
            self.push(lifetime, type_d(Op::LoadData, dst, u16::MAX));
            self.instructions.push(id);
        }
    }

    pub fn constant_string(&mut self, lifetime: SsaId, dst: u8, constant: ConstStringId) {
        let id: u32 = constant.into();
        if id < u16::MAX as u32 {
            self.push(lifetime, type_d(Op::LoadString, dst, id as u16));
        } else {
            self.push(lifetime, type_d(Op::LoadString, dst, u16::MAX));
            self.instructions.push(id);
        }
    }

    pub fn jump(&mut self, id: SsaId, target: SsaId, condition: Option<(u8, bool)>) {
        self.jumps.push((self.instructions.len(), target));
        if let Some((v, t)) = condition {
            if t {
                self.push(id, type_d(Op::JumpTrue, v, 0));
            } else {
                self.push(id, type_d(Op::JumpFalse, v, 0));
            }
        } else {
            self.push(id, type_d(Op::Jump, 0, 0));
        }
    }

    pub fn finish(mut self, used_registers: u8, used_environment_slots: u32) -> ModuleFunction {
        self.instructions[self.offset as usize] = type_d(Op::StackPush, used_registers, 0);
        for s in self.returns {
            let instr = self.instructions[s];
            match op_op(instr) {
                op::Return => {
                    let d = op_d(instr);
                    self.instructions[s] = type_d(Op::Return, used_registers, d);
                }
                op::ReturnUndefined => {
                    self.instructions[s] = type_d(Op::ReturnUndefined, used_registers, 0);
                }
                _ => panic!("not a return"),
            }
        }

        let mut last = None;
        for i in (0..self.to_bc.len()).rev() {
            match self.to_bc.0[i] {
                Some(x) => last = Some(x),
                None => {
                    self.to_bc.0[i] = last;
                }
            }
        }

        for (idx, target) in self.jumps {
            let instr = self.instructions[idx];
            match op_op(instr) {
                op::JumpTrue => {
                    let a = op_a(instr);
                    let target: i32 = self.to_bc[target].unwrap().try_into().unwrap();
                    let cur: i32 = idx as i32;
                    let jump: i32 = target - cur - 1;
                    let target = (jump as u32).try_into().unwrap();
                    self.instructions[idx] = type_d(Op::JumpTrue, a, target);
                }
                op::JumpFalse => {
                    let a = op_a(instr);
                    let target: i32 = self.to_bc[target].unwrap().try_into().unwrap();
                    let cur: i32 = idx as i32;
                    let jump: i32 = target - cur - 1;
                    let target = (jump as u32).try_into().unwrap();
                    self.instructions[idx] = type_d(Op::JumpFalse, a, target);
                }
                op::Jump => {
                    let target: i32 = self.to_bc[target].unwrap().try_into().unwrap();
                    let cur: i32 = idx as i32;
                    let jump: i32 = target - cur - 1;
                    let target: i16 = jump.try_into().expect("jump did not have a target");
                    self.instructions[idx] = type_d(Op::Jump, 0, target as u16);
                }
                _ => panic!("not a jump"),
            }
        }
        ModuleFunction {
            offset: self.offset,
            len: self.instructions.len() as u32 - self.offset,
            slots: used_environment_slots,
            name: self.name,
        }
    }
}
