//! Bytecode format definition.
use super::{JSValue, Object};
use std::{fmt, mem};

#[macro_use]
mod macros;

pub type Instruction = u32;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum DataValue {
    String(String),
    Direct(JSValue),
}

pub struct Bytecode {
    pub instructions: Box<[Instruction]>,
    pub data: Box<[DataValue]>,
}

impl fmt::Display for Bytecode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "DATA:")?;
        for (idx, x) in self.data.iter().enumerate() {
            writeln!(f, "{}:{:?}", idx, x)?;
        }
        let mut cur = 0;
        writeln!(f, "\nINSTRUCTIONS:")?;
        while cur != self.instructions.len() {
            let instr = self.instructions[cur];
            format_instr(instr, f)?;
            write!(f, "")?;
            match op_op(instr) {
                op::CLD => {
                    if op_d(instr) == 0xffff {
                        cur += 1;
                        write!(f, ":{}", self.instructions[cur])?;
                    }
                    cur += 1;
                }
                _ => cur += 1,
            }
            writeln!(f, "")?;
        }
        Ok(())
    }
}

pub const OP_MASK: u32 = 0xff;
pub const A_MASK: u32 = 0xff << 8;
pub const B_MASK: u32 = 0xff << 16;
pub const C_MASK: u32 = 0xff << 24;
pub const D_MASK: u32 = 0xffff << 16;

/// Possible instruction types
#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub enum InstructionType {
    /// An instruction in the form of
    /// ```
    /// 0  2  4  6  8 10 12 14 16 18 20 22 24 26 28 30 32
    /// +-----------+-----------+-----------+-----------+
    /// |  OP CODE  |   REG A   |   REG B   |   REG C   |
    /// +-----------+-----------+-----------+-----------+
    /// ```
    A,
    /// An instruction in the form of
    /// ```
    /// 0  2  4  6  8 10 12 14 16 18 20 22 24 26 28 30 32
    /// +-----------+-----------+-----------------------+
    /// |  OP CODE  |   REG A   |         REG D         |
    /// +-----------+-----------+-----------------------+
    /// ```
    D,
}

/// Create a type A instruction
/// Checks for validity of instruction in debug mode
#[inline]
pub fn type_a(op: Op, a: u8, b: u8, c: u8) -> Instruction {
    debug_assert_eq!(get_type(op), InstructionType::A);
    (op as u32) | (a as u32) << 8 | (b as u32) << 16 | (c as u32) << 24
}

/// Create a type D instruction
/// Checks for validity of instruction in debug mode
#[inline]
pub fn type_d(op: Op, a: u8, d: u16) -> Instruction {
    debug_assert_eq!(get_type(op), InstructionType::D);
    op as u32 | (a as u32) << 8 | (d as u32) << 16
}

/// Get the OP code of an instruction
#[inline]
pub fn op_op(instr: Instruction) -> u8 {
    (instr & OP_MASK) as u8
}

/// Get A register of an instruction
#[inline]
pub fn op_a(instr: Instruction) -> u8 {
    ((instr & A_MASK) >> 8) as u8
}

/// Get B register of an instruction
#[inline]
pub fn op_b(instr: Instruction) -> u8 {
    ((instr & B_MASK) >> 16) as u8
}

/// Get C register of an instruction
#[inline]
pub fn op_c(instr: Instruction) -> u8 {
    ((instr & C_MASK) >> 24) as u8
}

/// Get D register of an instruction
#[inline]
pub fn op_d(instr: Instruction) -> u16 {
    ((instr & D_MASK) >> 16) as u16
}

op_code!(
    enum Op {
        /// load data constant with index operand D into reg A
        /// if the idx is u16::max the index for the constant is in in the next instruction slot
        CLD(dst, idx),

        /// set the entry from the key in reg D to the value from reg A in the global object
        OSET(obj, key, val),
        /// set reg A to value int the global object with the key in reg D.
        OGET(dst, obj, key),

        /// copy a value from one register to an other
        MOV(dst, src),

        /// Add the values from reg B and reg C and store them into reg A
        ADD(dst, op, op),
        /// Subtract the values from reg B and reg C and store them into reg A
        SUB(dst, op, op),
        /// Multiply the values from reg B and reg C and store them into reg A
        MUL(dst, op, op),
        /// Divide the values from reg B and reg C and store them into reg A
        DIV(dst, op, op),
        /// Modulo the values from reg B and reg C and store them into reg A
        MOD(dst, op, op),
        /// Power the values from reg B and reg C and store them into reg A
        POW(dst, op, op),
        /// Negate the value from the reg B and store in reg A
        NEG(dst, src),
        /// do the unaryPos operation on the value from the reg B and store in reg A
        POS(dst, src),
        /// Bitwise and
        BAND(dst, op, op),
        /// Bitwise or
        BOR(dst, op, op),
        /// Bitwise exclusive or
        BXOR(dst, op, op),

        SHL(dst, op, op),
        SHR(dst, op, op),
        USR(dst, op, op),

        EQ(dst, op, op),
        SEQ(dst, op, op),
        NEQ(dst, op, op),
        SNEQ(dst, op, op),

        /// return
        RET(src, null),

        /// return undefined
        RETU(null, null),
    }
);
