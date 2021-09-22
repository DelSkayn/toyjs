//! Bytecode format definition.

use crate::{
    gc::{Ctx, Gc, Trace},
    value::JSValue,
};
use std::{fmt, rc::Rc};
pub type Instruction = u32;

#[derive(Debug)]
pub struct RuntimeFunction {
    pub module: Gc<Module>,
    pub function: u32,
}

unsafe impl Trace for RuntimeFunction {
    fn trace(&self, ctx: Ctx) {
        ctx.mark(self.module);
    }
}

#[derive(Debug)]
pub struct ModuleFunction {
    pub offset: u32,
    pub len: u32,
    pub slots: u32,
    pub name: String,
}

#[derive(Debug)]
pub struct Module {
    pub bc: Bytecode,
    pub functions: Box<[ModuleFunction]>,
}

unsafe impl Trace for Module {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        false
    }
    fn trace(&self, ctx: Ctx) {}
}

#[derive(Debug)]
pub struct Bytecode {
    pub instructions: Box<[Instruction]>,
    pub data: Box<[JSValue]>,
    pub strings: Box<[String]>,
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "DATA:")?;
        for (idx, x) in self.bc.data.iter().enumerate() {
            writeln!(f, "{}:{:?}", idx, x)?;
        }
        writeln!(f, "\nSTRINGS:")?;
        for (idx, x) in self.bc.strings.iter().enumerate() {
            writeln!(f, "{}:{:?}", idx, x)?;
        }
        writeln!(f, "\nINSTRUCTIONS:")?;
        let n_chars = (self.bc.instructions.len() as f64).log10() as u64 + 1;
        for func in self.functions.iter() {
            let mut cur = func.offset;
            writeln!(f, "{}:", func.name)?;
            while cur != func.offset + func.len {
                write!(f, "\t")?;
                let instr = self.bc.instructions[cur as usize];
                format_instr(instr, f);
                let cur_n_chars = (cur as f64).log10() as u64 + 1;
                for _ in 0..n_chars - cur_n_chars {
                    write!(f, " ")?;
                }
                match op_op(instr) {
                    op::LoadData => {
                        if op_d(instr) == 0xffff {
                            cur += 1;
                            write!(f, " const:{}", self.bc.instructions[cur as usize])?;
                        }
                        cur += 1;
                    }
                    _ => cur += 1,
                }
                writeln!(f)?;
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
        CreateObject(dst, null),
        CreateFunction(dst, id),

        /// Load the global object in the destination register.
        LoadGlobal(dst, null),
        /// load data constant with index operand D into reg A
        /// if the idx is u16::max the index for the constant is in in the next instruction slot
        LoadData(dst, idx),
        /// load string constant with index operand D into reg A
        /// if the idx is u16::max the index for the constant is in in the next instruction slot
        LoadString(dst, idx),

        /// Push a register on to the stack
        StackPush(regs, null),
        /// Pop a value from the stack into a regster
        StackPop(dst, null),

        /// set the entry from the key in reg D to the value from reg A in the global object
        IndexAssign(obj, key, val),
        /// set reg A to value int the global object with the key in reg D.
        Index(dst, obj, key),

        /// Lookup an enviroment at a certain depth.
        GetEnv(dst, depth),
        /// set the entry from the key in reg D to the value from reg A in the global object
        EnvAssign(env, val, slot),
        /// set the entry from the key in reg D to the value from reg A in the global object
        EnvIndex(dst, env, slot),

        //// copy a value from one register to an other
        //Move(dst, src),
        /// Add the values from reg B and reg C and store them into reg A
        Add(dst, op, op),
        /// Subtract the values from reg B and reg C and store them into reg A
        Sub(dst, op, op),
        /// Multiply the values from reg B and reg C and store them into reg A
        Mul(dst, op, op),
        /// Divide the values from reg B and reg C and store them into reg A
        Div(dst, op, op),
        /// Modulo the values from reg B and reg C and store them into reg A
        Mod(dst, op, op),
        /// Power the values from reg B and reg C and store them into reg A
        Pow(dst, op, op),
        /// Negate the value from the reg B and store in reg A
        Negative(dst, src),
        /// Bitwise and
        BinaryAnd(dst, op, op),
        /// Bitwise or
        BinaryOr(dst, op, op),
        /// Bitwise exclusive or
        BinaryXor(dst, op, op),

        /// Convert the value to a number
        ToNumber(dst, src),
        /// Convert the value to a boolean
        ToBool(dst, src),
        /// Convert the value to a boolean
        IsNullish(dst, src),

        /// Shift left
        ShiftLeft(dst, op, op),
        /// Shift right
        ShiftRight(dst, op, op),
        /// Shift right unsigned
        ShiftUnsigned(dst, op, op),

        /// Equal
        Equal(dst, op, op),
        /// Strict Equal
        StrictEqual(dst, op, op),
        /// Not Equal
        NotEqual(dst, op, op),
        /// Strict Not Equal
        StrictNotEqual(dst, op, op),

        /// Greater
        Greater(dst, op, op),
        /// Greater or equal
        GreaterEqual(dst, op, op),
        /// Less
        Less(dst, op, op),
        /// Less or equal
        LessEqual(dst, op, op),

        /// Test if the condition is thruthy or falsey if neg is false or true respectively, and jump to the index at the next instruction slot
        JumpTrue(cond, tgt),
        JumpFalse(cond, tgt),
        /// jump to the index at the next instruction slot
        Jump(null, tgt),

        /// Call a function with a given number of arguments,
        /// Store returned value in dst
        Call(dst, func, num),

        /// return
        Return(regs, val),
        /// return undefined
        ReturnUndefined(regs, null),
    }
);
