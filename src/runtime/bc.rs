//! Bytecode format definition.
use super::{rc::RcVal, JSValue, Object};
use std::{fmt, mem};

#[macro_use]
mod macros;

pub type Instruction = u32;

pub struct Bytecode {
    pub instructions: Box<[Instruction]>,
    pub data: Box<[JSValue]>,
    pub strings: Box<[RcVal<String>]>,
}

impl fmt::Display for Bytecode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "DATA:")?;
        for (idx, x) in self.data.iter().enumerate() {
            writeln!(f, "{}:{:?}", idx, x)?;
        }
        writeln!(f, "\nSTRINGS:")?;
        for (idx, x) in self.strings.iter().enumerate() {
            writeln!(f, "{}:{:?}", idx, x.value)?;
        }
        let mut cur = 0;
        writeln!(f, "\nINSTRUCTIONS:")?;
        let n_chars = (self.instructions.len() as f64).log10() as u64 + 1;
        while cur != self.instructions.len() {
            let instr = self.instructions[cur];
            let cur_n_chars = (cur as f64).log10() as u64 + 1;
            for _ in 0..n_chars - cur_n_chars {
                write!(f, " ")?;
            }
            write!(f, "{}: ", cur)?;
            format_instr(instr, f)?;
            match op_op(instr) {
                op::CLD => {
                    if op_d(instr) == 0xffff {
                        cur += 1;
                        write!(f, " const:{}", self.instructions[cur])?;
                    }
                    cur += 1;
                }
                op::J | op::JCO => {
                    cur += 1;
                    write!(f, "  target:{}", self.instructions[cur])?;
                    cur += 1;
                }
                _ => cur += 1,
            }
            writeln!(f)?;
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
        /// load string constant with index operand D into reg A
        /// if the idx is u16::max the index for the constant is in in the next instruction slot
        CLS(dst, idx),

        PUSH(null, size),
        POP(null, null),
        /// Load a value from the stack
        LD(dst, idx),
        /// Push a value to the stack
        ST(src, idx),

        /// Load the global object in the destination register.
        LGB(dst, null),
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

        /// Convert the value to a boolean
        BOOL(dst, src),
        /// Convert the value to a boolean
        ISNUL(dst, src),

        /// Shift left
        SHL(dst, op, op),
        /// Shift right
        SHR(dst, op, op),
        /// Shift right unsigned
        USR(dst, op, op),

        /// Equal
        EQ(dst, op, op),
        /// Strict Equal
        SEQ(dst, op, op),
        /// Not Equal
        NEQ(dst, op, op),
        /// Strict Not Equal
        SNEQ(dst, op, op),

        /// Greater
        GE(dst, op, op),
        /// Greater or equal
        GEQ(dst, op, op),
        /// Less
        LE(dst, op, op),
        /// Less or equal
        LEQ(dst, op, op),

        /// Test if the condition is thruthy or falsey if neg is false or true respectively, and jump to the index at the next instruction slot
        JCO(cond, neg),
        /// jump to the index at the next instruction slot
        J(null, null),

        /// return
        RET(src, null),
        /// return undefined
        RETU(null, null),
    }
);
