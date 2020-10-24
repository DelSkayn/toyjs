use crate::{
    constants::{Constant, ConstantId, Constants},
    ssa::SsaId,
};
use bumpalo::{collections::Vec as BumpVec, Bump};
use common::{collections::HashMap, interner::Interner, newtype_vec};
use runtime::{
    bytecode::{op, op_a, op_d, op_op, type_d, Bytecode, Instruction, Op},
    value::JSValue,
};
use std::{
    convert::{TryFrom, TryInto},
    rc::Rc,
};

#[derive(Debug)]
pub struct SsaIdToBytecode<'alloc>(BumpVec<'alloc, Option<u32>>);
newtype_vec!(struct SsaIdToBytecode<'alloc,>[SsaId] -> Option<u32,>);

pub struct BytecodeBuilder<'a> {
    constants: &'a Constants,
    interner: &'a Interner,
    instructions: Vec<Instruction>,
    returns: Vec<usize>,
    jumps: Vec<(usize, SsaId)>,
    data: Vec<JSValue>,
    strings: Vec<String>,
    constant_id: HashMap<ConstantId, (u32, bool)>,
    to_bc: SsaIdToBytecode<'a>,
}

impl<'a> BytecodeBuilder<'a> {
    pub fn new(alloc: &'a Bump, constants: &'a Constants, interner: &'a Interner) -> Self {
        let to_bc = SsaIdToBytecode(BumpVec::new_in(alloc));
        BytecodeBuilder {
            interner,
            constants,
            instructions: vec![0],
            returns: Vec::new(),
            jumps: Vec::new(),
            data: Vec::new(),
            strings: Vec::new(),
            constant_id: HashMap::default(),
            to_bc,
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
        self.instructions.push(instruction)
    }

    pub fn constant(&mut self, lifetime: SsaId, dst: u8, constant: ConstantId) {
        let strings = &mut self.strings;
        let data = &mut self.data;
        let interner = &self.interner;
        let constants = &self.constants;

        let (id, string) =
            *self
                .constant_id
                .entry(constant)
                .or_insert_with(|| match constants.lookup(constant) {
                    Constant::String(x) => {
                        let id = u32::try_from(strings.len()).unwrap();
                        strings.push(interner.lookup(*x).unwrap().to_string());
                        (id, true)
                    }
                    Constant::Null => {
                        let id = u32::try_from(data.len()).unwrap();
                        data.push(JSValue::null());
                        (id, false)
                    }
                    Constant::Undefined => {
                        let id = u32::try_from(data.len()).unwrap();
                        data.push(JSValue::undefined());
                        (id, false)
                    }
                    Constant::Float(x) => {
                        let id = u32::try_from(data.len()).unwrap();
                        data.push(JSValue::from(*x));
                        (id, false)
                    }
                    Constant::Integer(x) => {
                        let id = u32::try_from(data.len()).unwrap();
                        data.push(JSValue::from(*x));
                        (id, false)
                    }
                    Constant::Boolean(x) => {
                        let id = u32::try_from(data.len()).unwrap();
                        data.push(JSValue::from(*x));
                        (id, false)
                    }
                });
        let op = if string { Op::LoadString } else { Op::LoadData };
        if id < u16::MAX as u32 {
            self.push(lifetime, type_d(op, dst, id as u16));
        } else {
            self.push(lifetime, type_d(op, dst, u16::MAX));
            self.instructions.push(id);
        }
    }

    pub fn jump(&mut self, id: SsaId, target: SsaId, condition: Option<u8>) {
        self.jumps.push((self.instructions.len(), target));
        if let Some(x) = condition {
            self.push(id, type_d(Op::ConditionalJump, x, 0));
        } else {
            self.push(id, type_d(Op::Jump, 0, 0));
        }
    }

    pub fn finish(mut self, used_registers: u8, used_environment_slots: u32) -> Bytecode {
        self.instructions[0] = type_d(Op::StackPush, used_registers, 0);
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
                op::ConditionalJump => {
                    let a = op_a(instr);
                    let target: i32 = self.to_bc[target].unwrap().try_into().unwrap();
                    let cur: i32 = idx as i32;
                    let jump: i32 = target - cur - 1;
                    let target = (jump as u32).try_into().unwrap();
                    self.instructions[idx] = type_d(Op::ConditionalJump, a, target);
                }
                op::Jump => {
                    let target: i32 = self.to_bc[target].unwrap().try_into().unwrap();
                    let cur: i32 = idx as i32;
                    let jump: i32 = target - cur - 1;
                    let target = (jump as u32).try_into().unwrap();
                    self.instructions[idx] = type_d(Op::Jump, 0, target);
                }
                _ => panic!("not a jump"),
            }
        }
        Bytecode {
            slots: used_environment_slots,
            instructions: self.instructions.into_boxed_slice(),
            data: self.data.into_boxed_slice(),
            strings: self.strings.into_boxed_slice(),
        }
    }
}
