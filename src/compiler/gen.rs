use crate::{
    compiler::*,
    interner::Interner,
    runtime::{
        self,
        bc::{self, Bytecode},
        rc::RcVal,
        JSValue,
    },
    ssa::{BinOp, Constant, Instruction, Ssa, UnaryOp},
};
use fxhash::FxHashMap;
use std::{cell::Cell, collections::VecDeque};

impl Compiler {
    pub fn generate_bytecode(ssa: &Ssa, register_alloc: &[u8], interner: &Interner) -> Bytecode {
        let mut pending_targets = VecDeque::<(u32, usize)>::new();
        let mut ssa_to_bytecode = Vec::new();

        let mut data_idx = FxHashMap::default();
        let mut string_idx = FxHashMap::default();

        let mut data = Vec::new();
        let mut strings = Vec::new();

        enum StringOrData {
            String(u32),
            Data(u32),
        }

        let mut load_constant = |constant| {
            let js_value = match ssa.constants[constant as usize] {
                Constant::String(x) => {
                    let res = string_idx.entry(x).or_insert_with(|| {
                        let idx = strings.len();
                        let string_value = RcVal {
                            count: Cell::new(usize::MAX),
                            value: interner.lookup(x).unwrap().to_string(),
                        };
                        strings.push(string_value);
                        idx as u32
                    });
                    return StringOrData::String(*res);
                }
                Constant::Float(x) => JSValue::from(x),
                Constant::Integer(x) => JSValue::from(x),
                Constant::Boolean(x) => JSValue::from(x),
                Constant::Undefined => JSValue::undefined(),
                Constant::Null => JSValue::null(),
            };
            let res = data_idx.entry(js_value).or_insert_with(|| {
                let res = data.len();
                data.push(js_value);
                res as u32
            });
            StringOrData::Data(*res)
        };

        let mut instructions = Vec::with_capacity(ssa.instructions.len());
        ssa.instructions
            .iter()
            .enumerate()
            .for_each(|(idx, instr)| {
                ssa_to_bytecode.push(instructions.len() as u32);
                loop {
                    if let Some(x) = pending_targets.front() {
                        if x.0 <= idx as u32 {
                            instructions[x.1] =
                                (ssa_to_bytecode[x.0 as usize] as i32 - (x.1 as i32 + 1)) as u32;
                            pending_targets.pop_front();
                            continue;
                        }
                    }
                    break;
                }
                match *instr {
                    Instruction::LoadGlobal => {
                        let dst = register_alloc[idx];
                        instructions.push(bc::type_d(bc::Op::LoadGlobal, dst, 0));
                    }
                    Instruction::ObjectSet { object, key, value } => {
                        let obj = register_alloc[object.as_u32() as usize];
                        let val = register_alloc[value.as_u32() as usize];
                        let key = register_alloc[key.as_u32() as usize];
                        instructions.push(bc::type_a(bc::Op::ObjectSet, obj, key, val));
                    }
                    Instruction::ObjectGet { object, key } => {
                        let key = register_alloc[key.as_u32() as usize];
                        let obj = register_alloc[object.as_u32() as usize];
                        let dst = register_alloc[idx];
                        instructions.push(bc::type_a(bc::Op::ObjectGet, dst, obj, key));
                    }
                    Instruction::Move { operand } => {
                        let src = register_alloc[operand.as_u32() as usize];
                        let dst = register_alloc[idx];
                        instructions.push(bc::type_d(bc::Op::Move, dst, src as u16));
                    }
                    Instruction::Binary { kind, left, right } => {
                        let op = match kind {
                            BinOp::Add => bc::Op::Add,
                            BinOp::Subtract => bc::Op::Sub,
                            BinOp::Multiply => bc::Op::Mul,
                            BinOp::Divide => bc::Op::Div,
                            BinOp::Modulo => bc::Op::Mod,
                            BinOp::Power => bc::Op::Pow,
                            BinOp::BitwiseOr => bc::Op::BinaryOr,
                            BinOp::BitwiseAnd => bc::Op::BinaryAnd,
                            BinOp::BitwiseXor => bc::Op::BinaryXor,
                            BinOp::ShiftLeft => bc::Op::ShiftLeft,
                            BinOp::ShiftRight => bc::Op::ShiftRight,
                            BinOp::ShiftRightUnsigned => bc::Op::ShiftUnsigned,
                            BinOp::Equal => bc::Op::Equal,
                            BinOp::StrictEqual => bc::Op::StrictEqual,
                            BinOp::NotEqual => bc::Op::NotEqual,
                            BinOp::StrictNotEqual => bc::Op::StrictNotEqual,
                            BinOp::Less => bc::Op::Less,
                            BinOp::LessEqual => bc::Op::LessEqual,
                            BinOp::Greater => bc::Op::Greater,
                            BinOp::GreaterEqual => bc::Op::GreaterEqual,
                            _ => todo!(),
                        };
                        let left = register_alloc[left.as_u32() as usize];
                        let right = register_alloc[right.as_u32() as usize];
                        let dest = register_alloc[idx];
                        instructions.push(bc::type_a(op, dest, left, right));
                    }
                    Instruction::Unary { kind, operand } => {
                        let op = match kind {
                            UnaryOp::Postive => bc::Op::Positive,
                            UnaryOp::Negative => bc::Op::Negative,
                            UnaryOp::AddOne => bc::Op::AddOne,
                            UnaryOp::SubtractOne => bc::Op::SubOne,
                            UnaryOp::ToBool => bc::Op::ToBool,
                            UnaryOp::IsNullish => bc::Op::IsNullish,
                            _ => todo!(),
                        };
                        let operand = register_alloc[operand.as_u32() as usize];
                        let dest = register_alloc[idx];
                        instructions.push(bc::type_d(op, dest, operand as u16));
                    }
                    Instruction::Return { value } => {
                        if value == InstrVar::null() {
                            instructions.push(bc::type_d(bc::Op::ReturnUndefined, 0, 0));
                        } else {
                            let operand = register_alloc[value.as_u32() as usize];
                            instructions.push(bc::type_d(bc::Op::Return, operand, 0));
                        }
                    }
                    Instruction::LoadConstant { constant } => {
                        let (op, const_id) = match load_constant(constant.0) {
                            StringOrData::Data(x) => (bc::Op::LoadData, x),
                            StringOrData::String(x) => (bc::Op::LoadString, x),
                        };
                        let const_idx = const_id.min(u16::max_value() as u32) as u16;
                        let dest = register_alloc[idx];
                        instructions.push(bc::type_d(op, dest, const_idx));
                        if const_idx == u16::max_value() {
                            instructions.push(const_id);
                        }
                    }
                    Instruction::Jump { target } => {
                        assert!(target != InstrVar::null());
                        instructions.push(bc::type_d(bc::Op::Jump, 0, 0));
                        if target.as_u32() as usize > idx {
                            pending_targets.push_back((target.as_u32(), instructions.len()));
                            instructions.push(0);
                        } else {
                            let target = (ssa_to_bytecode[target.as_u32() as usize] as i32)
                                .checked_sub(ssa_to_bytecode[idx] as i32 + 2)
                                .expect("jump_target to big!");
                            instructions.push(target as u32);
                        }
                    }
                    Instruction::Alias { left: _, right: _ } => {}
                    Instruction::CondJump {
                        negative,
                        target,
                        condition,
                    } => {
                        assert!(target != InstrVar::null());
                        let cond = register_alloc[condition.as_u32() as usize] as u8;
                        instructions.push(bc::type_d(
                            bc::Op::ConditionalJump,
                            cond,
                            negative as u16,
                        ));
                        if target.as_u32() as usize > idx {
                            pending_targets.push_back((target.as_u32(), instructions.len()));
                            instructions.push(0);
                        } else {
                            let target = (ssa_to_bytecode[target.as_u32() as usize] as i32)
                                .checked_sub(ssa_to_bytecode[idx] as i32 + 2)
                                .expect("jump_target to big!");
                            instructions.push(target as u32);
                        }
                    }
                }
            });
        Bytecode {
            instructions: instructions.into_boxed_slice(),
            data: data.into_boxed_slice(),
            strings: strings.into_boxed_slice(),
        }
    }
}
