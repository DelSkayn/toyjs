use crate::{
    constants::{Constant, ConstantId, Constants},
    ssa::SsaId,
};
use common::{collections::HashMap, interner::Interner};
use runtime::{
    bytecode::{op, op_d, op_op, type_d, Bytecode, Instruction, Op},
    value::JSValue,
};
use std::{convert::TryFrom, rc::Rc};

pub struct BytecodeBuilder<'a> {
    constants: &'a Constants,
    interner: &'a Interner,
    instructions: Vec<Instruction>,
    returns: Vec<usize>,
    data: Vec<JSValue>,
    strings: Vec<String>,
    constant_id: HashMap<ConstantId, (u32, bool)>,
}

impl<'a> BytecodeBuilder<'a> {
    pub fn new(constants: &'a Constants, interner: &'a Interner) -> Self {
        BytecodeBuilder {
            interner,
            constants,
            instructions: vec![0],
            returns: Vec::new(),
            data: Vec::new(),
            strings: Vec::new(),
            constant_id: HashMap::default(),
        }
    }

    pub fn push(&mut self, _id: SsaId, instruction: Instruction) {
        if op_op(instruction) == op::Return || op_op(instruction) == op::ReturnUndefined {
            self.returns.push(self.instructions.len());
        }
        self.instructions.push(instruction)
    }

    pub fn constant(&mut self, _lifetime: SsaId, dst: u8, constant: ConstantId) {
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
            self.instructions.push(type_d(op, dst, id as u16))
        } else {
            self.instructions.push(type_d(op, dst, u16::MAX));
            self.instructions.push(id);
        }
    }

    pub fn finish(mut self, used_registers: u8) -> Bytecode {
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
        Bytecode {
            instructions: self.instructions.into_boxed_slice(),
            data: self.data.into_boxed_slice(),
            strings: self.strings.into_boxed_slice(),
        }
    }
}
