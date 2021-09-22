use crate::{
    constants::{Constant, Constants},
    ssa::{BinaryOperation, Ssa, SsaId, SsaVec, UnaryOperation},
};
use bumpalo::Bump;
use common::interner::Interner;
use runtime::{
    bytecode::{op, type_a, type_d, Bytecode, Instruction, Module, ModuleFunction, Op},
    value::JSValue,
};
use std::{cmp::Ord, convert::TryFrom};

mod register_allocator;
use register_allocator::RegisterAllocator;

mod bytecode_builder;
use bytecode_builder::BytecodeBuilder;

pub struct ModuleBuilder {
    pub instructions: Vec<Instruction>,
    pub constants: Constants,
    pub functions: Vec<ModuleFunction>,
}

impl ModuleBuilder {
    pub fn new() -> Self {
        ModuleBuilder {
            instructions: Vec::new(),
            constants: Constants::new(),
            functions: Vec::new(),
        }
    }

    pub fn build(self, interner: &Interner) -> Module {
        let mut data = Vec::with_capacity(self.constants.ids.len());
        self.constants
            .ids
            .into_iter()
            .map(|(k, v)| {
                let k = match k {
                    Constant::Float(x) => JSValue::from(x),
                    Constant::Integer(x) => JSValue::from(x),
                    Constant::Boolean(x) => JSValue::from(x),
                    Constant::Null => JSValue::null(),
                    Constant::Undefined => JSValue::undefined(),
                };
                let v: u32 = v.into();
                (v, k)
            })
            .for_each(|(k, v)| {
                for _ in data.len()..(k as usize + 1) {
                    data.push(JSValue::undefined())
                }
                data[k as usize] = v
            });

        let mut strings = Vec::with_capacity(self.constants.string_ids.len());
        self.constants
            .string_ids
            .into_iter()
            .map(|(k, v)| {
                let k = interner.lookup(k).unwrap().to_string();
                let v: u32 = v.into();
                (v, k)
            })
            .for_each(|(k, v)| {
                for _ in strings.len()..(k as usize + 1) {
                    strings.push(String::new())
                }
                strings[k as usize] = v
            });

        let strings = strings.into_boxed_slice();
        let data = data.into_boxed_slice();
        let instructions = self.instructions.into_boxed_slice();
        let functions = self.functions.into_boxed_slice();
        Module {
            bc: Bytecode {
                instructions,
                data,
                strings,
            },
            functions,
        }
    }
}

pub struct Generator<'a> {
    interner: &'a Interner,
    ssa: &'a SsaVec,
    allocator: RegisterAllocator<'a>,
    pending_jumps: Vec<usize>,
    functions: &'a mut Vec<ModuleFunction>,
    builder: BytecodeBuilder<'a>,
    num_slots: u32,
}

impl<'a> Generator<'a> {
    pub fn new(
        name: String,
        ssa: &'a SsaVec,
        interner: &'a Interner,
        module_builder: &'a mut ModuleBuilder,
        num_slots: u32,
    ) -> Self {
        Generator {
            allocator: RegisterAllocator::new(ssa),
            interner,
            pending_jumps: Vec::new(),
            ssa,
            functions: &mut module_builder.functions,
            builder: BytecodeBuilder::new(
                name,
                &module_builder.constants,
                interner,
                &mut module_builder.instructions,
            ),
            num_slots,
        }
    }

    pub fn generate(mut self) -> u32 {
        for i in 0..self.ssa.len() {
            let id = SsaId::from(i);
            self.generate_instruction(id);
        }
        let func = self
            .builder
            .finish(self.allocator.used_registers(), self.num_slots);
        let res = self.functions.len() as u32;
        self.functions.push(func);
        res
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
            Ssa::CreateFunction { function } => {
                if !self.allocator.is_used(ssa) {
                    return None;
                }
                let dst = self.allocator.allocate(ssa);
                self.builder.create_function(ssa, dst, function);
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
            Ssa::LoadString { constant } => {
                if !self.allocator.is_used(ssa) {
                    return None;
                }
                let dst = self.allocator.allocate(ssa);
                self.builder.constant_string(ssa, dst, constant);
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
            Ssa::Jump { to } => {
                let to = to.unwrap();
                self.builder.jump(ssa, to, None);
                None
            }
            Ssa::ConditionalJump {
                condition,
                to,
                jump_true,
            } => {
                let to = to.unwrap();
                let cond = self.allocator.retrieve_register(condition);
                self.builder.jump(ssa, to, Some((cond, jump_true)));
                None
            }

            Ssa::Alias { .. } => None,
            _ => todo!(),
        }
    }
}
