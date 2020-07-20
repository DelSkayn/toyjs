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

impl Compiler {
    pub fn generate_bytecode(ssa: &Ssa, register_alloc: &Vec<u8>, interner: &Interner) -> Bytecode {
        let mut instructions = Vec::with_capacity(ssa.instructions.len());
        let constants = Self::generate_constants(ssa, interner);
        ssa.instructions
            .iter()
            .enumerate()
            .for_each(|(idx, instr)| match *instr {
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
                Instruction::Jump { target: _ } => todo!(),
                Instruction::Alias { left: _, right: _ } => todo!(),
                Instruction::CondJump {
                    negative: _,
                    target: _,
                    condition: _,
                } => todo!(),
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
