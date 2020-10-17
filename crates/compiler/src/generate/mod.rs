use crate::{
    constants::Constants,
    ssa::{BinaryOperation, Ssa, SsaId, SsaVec, UnaryOperation},
};
use bumpalo::{collections::Vec, Bump};
use common::interner::Interner;
use runtime::bytecode::{op, type_a, type_d, Bytecode, Op};
use std::convert::TryFrom;

mod register_allocator;
use register_allocator::RegisterAllocator;

mod bytecode_builder;
use bytecode_builder::BytecodeBuilder;

pub struct Generator<'a, 'alloc> {
    alloc: &'alloc Bump,
    interner: &'a Interner,
    ssa: &'a SsaVec<'alloc>,
    constants: &'a Constants,
    allocator: RegisterAllocator<'a, 'alloc>,
    pending_jumps: Vec<'alloc, usize>,
    builder: BytecodeBuilder<'a>,
    num_slots: u32,
}

impl<'a, 'alloc> Generator<'a, 'alloc> {
    pub fn new(
        alloc: &'alloc Bump,
        ssa: &'a SsaVec<'alloc>,
        constants: &'a Constants,
        interner: &'a Interner,
        num_slots: u32,
    ) -> Self {
        Generator {
            allocator: RegisterAllocator::new(alloc, ssa),
            interner,
            pending_jumps: Vec::new_in(alloc),
            ssa,
            constants,
            alloc,
            builder: BytecodeBuilder::new(constants, interner),
            num_slots,
        }
    }

    pub fn generate(mut self) -> Bytecode {
        for i in 0..self.ssa.len() {
            let id = SsaId::from(i);
            self.generate_instruction(id);
        }
        self.builder
            .finish(self.allocator.used_registers(), self.num_slots)
    }

    fn generate_instruction(&mut self, ssa: SsaId) -> Option<u8> {
        match self.ssa[ssa] {
            Ssa::GetGlobal => {
                if !self.allocator.is_used(ssa) {
                    return None;
                }
                let dst = self.allocator.allocate(ssa);
                self.builder.push(ssa, type_d(Op::LoadGlobal, dst, 0));
                Some(dst)
            }
            Ssa::CreateObject => {
                if !self.allocator.is_used(ssa) {
                    return None;
                }
                let dst = self.allocator.allocate(ssa);
                self.builder.push(ssa, type_d(Op::CreateObject, dst, 0));
                Some(dst)
            }
            Ssa::CreateEnvironment => {
                if !self.allocator.is_used(ssa) {
                    return None;
                }
                todo!()
            }
            Ssa::LoadConstant { constant } => {
                if !self.allocator.is_used(ssa) {
                    return None;
                }
                let dst = self.allocator.allocate(ssa);
                self.builder.constant(ssa, dst, constant);
                Some(dst)
            }
            Ssa::Return { expr } => {
                if let Some(expr) = expr {
                    let val = self.allocator.retrieve_register(expr);
                    self.builder.push(ssa, type_d(Op::Return, 0, val as u16));
                } else {
                    self.builder.push(ssa, type_d(Op::ReturnUndefined, 0, 0));
                }
                None
            }
            Ssa::Binary { op, left, right } => {
                let left = self.allocator.retrieve_register(left);
                let right = self.allocator.retrieve_register(right);
                let dest = self.allocator.allocate(ssa);
                let op = match op {
                    BinaryOperation::Add => Op::Add,
                    BinaryOperation::Subtract => Op::Sub,
                    BinaryOperation::Multiply => Op::Mul,
                    BinaryOperation::Divide => Op::Div,
                    BinaryOperation::Modulo => Op::Mod,
                    BinaryOperation::Exponentiate => Op::Pow,
                    BinaryOperation::Equal => Op::Equal,
                    BinaryOperation::NotEqual => Op::NotEqual,
                    BinaryOperation::StrictEqual => Op::StrictEqual,
                    BinaryOperation::StrictNotEqual => Op::StrictNotEqual,
                    BinaryOperation::Less => Op::Less,
                    BinaryOperation::LessEqual => Op::LessEqual,
                    BinaryOperation::Greater => Op::Greater,
                    BinaryOperation::GreaterEqual => Op::GreaterEqual,
                    BinaryOperation::LeftShift => Op::ShiftLeft,
                    BinaryOperation::RightShift => Op::ShiftRight,
                    BinaryOperation::UnsignedRightShift => Op::ShiftUnsigned,
                    _ => todo!(),
                };
                self.builder.push(ssa, type_a(op, dest, left, right));
                Some(dest)
            }
            Ssa::Unary { op, operand } => {
                let operand = self.allocator.retrieve_register(operand);
                let dst = self.allocator.allocate(ssa);
                let op = match op {
                    UnaryOperation::Not => todo!(),
                    UnaryOperation::Negative => Op::Negative,
                    UnaryOperation::ToNumber => Op::ToNumber,
                    _ => todo!(),
                };
                self.builder.push(ssa, type_d(op, dst, operand as u16));
                Some(dst)
            }
            Ssa::Index { object, key } => {
                let object = self.allocator.retrieve_register(object);
                let key = self.allocator.retrieve_register(key);
                let dst = self.allocator.allocate(ssa);
                self.builder.push(ssa, type_a(Op::Index, dst, object, key));
                Some(dst)
            }
            Ssa::Assign { object, key, value } => {
                let object = self.allocator.retrieve_register(object);
                let key = self.allocator.retrieve_register(key);
                let value = self.allocator.retrieve_register(value);
                self.builder
                    .push(ssa, type_a(Op::IndexAssign, object, key, value));
                None
            }
            Ssa::IndexEnvironment { env, slot } => {
                let env = self.allocator.retrieve_register(env);
                let dst = self.allocator.allocate(ssa);
                if slot >= u8::MAX as u32 {
                    todo!();
                }
                self.builder
                    .push(ssa, type_a(Op::EnvIndex, dst, env, slot as u8));
                Some(dst)
            }
            Ssa::AssignEnvironment { value, env, slot } => {
                let value = self.allocator.retrieve_register(value);
                let env = self.allocator.retrieve_register(env);
                let dst = self.allocator.allocate(ssa);
                if slot >= u8::MAX as u32 {
                    todo!();
                }
                self.builder
                    .push(ssa, type_a(Op::EnvAssign, env, value, slot as u8));
                Some(dst)
            }
            Ssa::GetEnvironment { depth } => {
                let dst = self.allocator.allocate(ssa);
                if depth >= u16::MAX as u32 {
                    todo!()
                }
                self.builder
                    .push(ssa, type_d(Op::GetEnv, dst, depth as u16));
                None
            }
            _ => todo!(),
        }
    }
}
