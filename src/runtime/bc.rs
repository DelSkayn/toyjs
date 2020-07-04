use super::JSValue;
use std::{fmt, mem};

#[macro_use]
mod macros;

pub type Instruction = u32;

pub const PRIM_VAL_FALSE: u16 = 0;
pub const PRIM_VAL_TRUE: u16 = 1;
pub const PRIM_VAL_NULL: u16 = 2;
pub const PRIM_VAL_UNDEFINED: u16 = 3;

pub struct Bytecode {
    pub instructions: Box<[Instruction]>,
    pub data: Box<[JSValue]>,
}

impl Drop for Bytecode {
    fn drop(&mut self) {
        for d in self.data.iter() {
            unsafe { d.drop() }
        }
    }
}

impl fmt::Display for Bytecode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut cur = 0;
        writeln!(f, "DATA:")?;
        for (idx, x) in self.data.iter().enumerate() {
            writeln!(f, "{}:{}", idx, x)?;
        }
        writeln!(f, "\nINSTRUCTIONS:")?;
        while cur != self.instructions.len() {
            let instr = self.instructions[cur];
            format_instr(instr, f)?;
            writeln!(f, "")?;
            match op_op(instr) {
                op::CLF => {
                    cur += 3;
                }
                _ => cur += 1,
            }
        }
        Ok(())
    }
}

pub const OP_MASK: u32 = 0xff;
pub const A_MASK: u32 = 0xff << 8;
pub const B_MASK: u32 = 0xff << 16;
pub const C_MASK: u32 = 0xff << 24;
pub const D_MASK: u32 = 0xffff << 16;

#[derive(Eq, PartialEq, Debug)]
enum InstructionType {
    A,
    D,
}

#[inline]
pub fn type_a(op: u8, a: u8, b: u8, c: u8) -> Instruction {
    debug_assert_eq!(get_type(op), InstructionType::A);
    (op as u32) | (a as u32) << 8 | (b as u32) << 16 | (c as u32) << 24
}

#[inline]
pub fn type_d(op: u8, a: u8, d: u16) -> Instruction {
    debug_assert_eq!(get_type(op), InstructionType::D);
    op as u32 | (a as u32) << 8 | (d as u32) << 16
}

#[inline]
pub fn op_op(instr: Instruction) -> u8 {
    (instr & OP_MASK) as u8
}

#[inline]
pub fn op_a(instr: Instruction) -> u8 {
    ((instr & A_MASK) >> 8) as u8
}

#[inline]
pub fn op_b(instr: Instruction) -> u8 {
    ((instr & B_MASK) >> 16) as u8
}

#[inline]
pub fn op_c(instr: Instruction) -> u8 {
    ((instr & C_MASK) >> 24) as u8
}

#[inline]
pub fn op_d(instr: Instruction) -> u16 {
    ((instr & D_MASK) >> 16) as u16
}

op_code!(
    enum Op {
        /// store value from operand D into the lower bits of reg A
        CLH(dst, val),
        /// store value from operand D into the higher bits of reg A
        CLL(dst, val),
        /// load constant float value from the next two register values to reg A
        CLF(dst, null),
        /// load constant primitive value operand D and store them into reg A
        CLP(dst, prim),
        /// load data constant with index operand D into reg A
        /// if the last bit of the idx is set the value is not a direct index but a register for and offset;
        CLD(dst, idx),

        /// set the entry from the key in reg D to the value from reg A in the global object
        GSET(val, key),
        /// set reg A to value int the global object with the key in reg D.
        GGET(dst, key),

        /// copy a value from one register to an other
        MOV(dst, src),

        /// Add the values from reg A and reg B and store them into reg C
        ADD(dst, op, op),
        /// Add the values from reg A and reg B and store them into reg C
        SUB(dst, op, op),
        /// Add the values from reg A and reg B and store them into reg C
        MUL(dst, op, op),
        /// Add the values from reg A and reg B and store them into reg C
        DIV(dst, op, op),
        /// Add the values from reg A and reg B and store them into reg C
        MOD(dst, op, op),
        /// Add the values from reg A and reg B and store them into reg C
        POW(dst, op, op),
        /// return
        RET(src, null),
    }
);
