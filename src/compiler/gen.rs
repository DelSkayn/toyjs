use crate::{
    compiler::*,
    interner::Interner,
    runtime::{
        self,
        bc::{self, Bytecode},
        JSValue,
    },
    ssa::{BinOp, Constant, InstrVar, Instruction, Ssa, UnaryOp},
};
use std::collections::VecDeque;

impl Compiler {
    pub fn generate_bytecode(ssa: &Ssa, register_alloc: &Vec<u8>, interner: &Interner) -> Bytecode {
        let mut pending_targets = VecDeque::<(u32, usize)>::new();
        let mut ssa_to_bytecode = Vec::new();

        let mut instructions = Vec::with_capacity(ssa.instructions.len());
        let constants = Self::generate_constants(ssa, interner);
        ssa.instructions
            .iter()
            .enumerate()
            .for_each(|(idx, instr)| {
                ssa_to_bytecode.push(instructions.len() as u32);
                loop {
                    if let Some(x) = pending_targets.front() {
                        if x.0 <= idx as u32 {
                            instructions[x.1] = ssa_to_bytecode[x.0 as usize];
                            pending_targets.pop_front();
                            continue;
                        }
                    }
                    break;
                }
                match *instr {
                    Instruction::Binary { kind, left, right } => {
                        let op = match kind {
                            BinOp::Add => bc::Op::ADD,
                            BinOp::Subtract => bc::Op::SUB,
                            BinOp::Multiply => bc::Op::MUL,
                            BinOp::Divide => bc::Op::DIV,
                            BinOp::Modulo => bc::Op::MOD,
                            BinOp::Power => bc::Op::POW,
                            BinOp::BitwiseOr => bc::Op::BOR,
                            BinOp::BitwiseAnd => bc::Op::BAND,
                            BinOp::BitwiseXor => bc::Op::BXOR,
                            BinOp::ShiftLeft => bc::Op::SHL,
                            BinOp::ShiftRight => bc::Op::SHR,
                            BinOp::ShiftRightUnsigned => bc::Op::USR,
                            BinOp::Equal => bc::Op::EQ,
                            BinOp::StrictEqual => bc::Op::SEQ,
                            BinOp::NotEqual => bc::Op::NEQ,
                            BinOp::StrictNotEqual => bc::Op::SNEQ,
                            _ => todo!(),
                        };
                        let left = register_alloc[left.0 as usize];
                        let right = register_alloc[right.0 as usize];
                        let dest = register_alloc[idx];
                        instructions.push(bc::type_a(op, dest, left, right));
                    }
                    Instruction::Unary { kind, operand } => {
                        let op = match kind {
                            UnaryOp::Postive => bc::Op::POS,
                            UnaryOp::Negative => bc::Op::NEG,
                            UnaryOp::ToBool => bc::Op::BOOL,
                            UnaryOp::IsNullish => bc::Op::ISNUL,
                            _ => todo!(),
                        };
                        let operand = register_alloc[operand.0 as usize];
                        let dest = register_alloc[idx];
                        instructions.push(bc::type_d(op, dest, operand as u16));
                    }
                    Instruction::Return { value } => {
                        if value == InstrVar::null() {
                            instructions.push(bc::type_d(bc::Op::RETU, 0, 0));
                        } else {
                            let operand = register_alloc[value.0 as usize];
                            instructions.push(bc::type_d(bc::Op::RET, operand, 0));
                        }
                    }
                    Instruction::LoadConstant { constant } => {
                        let const_id = constant.0;
                        let const_idx = const_id.min(u16::max_value() as u32) as u16;
                        let dest = register_alloc[idx];
                        instructions.push(bc::type_d(bc::Op::CLD, dest, const_idx));
                        if const_idx == u16::max_value() {
                            instructions.push(const_id);
                        }
                    }
                    Instruction::Jump { target } => {
                        instructions.push(bc::type_d(bc::Op::J, 0, 0));
                        if target.0 as usize > idx {
                            pending_targets.push_back((target.0, instructions.len()));
                            instructions.push(0);
                        } else {
                            instructions.push(ssa_to_bytecode[target.0 as usize]);
                        }
                    }
                    Instruction::Alias { left: _, right: _ } => {}
                    Instruction::CondJump {
                        negative,
                        target,
                        condition,
                    } => {
                        let cond = register_alloc[condition.0 as usize] as u8;
                        instructions.push(bc::type_d(bc::Op::JCO, cond, negative as u16));
                        if target.0 as usize > idx {
                            pending_targets.push_back((target.0, instructions.len()));
                            instructions.push(0);
                        } else {
                            instructions.push(ssa_to_bytecode[target.0 as usize]);
                        }
                    }
                }
            });
        Bytecode {
            instructions: instructions.into_boxed_slice(),
            data: constants.into_boxed_slice(),
        }
    }

    fn generate_constants(ssa: &Ssa, interner: &Interner) -> Vec<bc::DataValue> {
        ssa.constants
            .iter()
            .map(|c| match *c {
                Constant::Null => bc::DataValue::Direct(JSValue::null()),
                Constant::Undefined => bc::DataValue::Direct(JSValue::undefined()),
                Constant::Float(x) => bc::DataValue::Direct(JSValue::from(x)),
                Constant::Integer(x) => bc::DataValue::Direct(JSValue::from(x)),
                Constant::Boolean(x) => bc::DataValue::Direct(JSValue::from(x)),
                Constant::String(x) => bc::DataValue::String(interner.lookup(x).to_string()),
            })
            .collect()
    }
}
