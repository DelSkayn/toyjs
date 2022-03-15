//! Module containing the instruction information
//!
//! Instructions come in two different formats.
//! 1. The simple to use enum representation in [`Instruction`]
//! 2. The dense format used in the runtime in the form of a series of bytes [`InstructionBuffer`]
//!
//! # Safety
//!
//! The vm makes a major assumption regarding the bytecode it is being handed to run, it assumes
//! the bytecode is correct.
//!
//! Bytecode which is correct should make sure that the following statements hold.
//!
//! - No pointer to an instruction, be it a jump or a offset into the instructions from a
//! bytefuction, should point to a value outside of the instruction buffer.
//! - All constants and functions id's in the bytecode should be valid id's for the current
//! bytecode.
//! - No instructions references a register larger then the amount of registers the current
//! function has defined.
//! - All instructions should be valid opcodes.
//! - All list of instructions belonging to a function should end in a return instruction.
//!

#[macro_use]
mod macros;
use crate::{
    gc::{self, Trace},
    Value,
};
use std::{error::Error, fmt};

#[derive(Debug)]
pub enum ValidationError {}

impl Error for ValidationError {}

impl fmt::Display for ValidationError {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {}
    }
}

#[derive(Clone, Debug, Copy)]
pub enum Upvalue {
    Local(u8),
    Parent(u16),
}

#[derive(Clone, Debug)]
pub struct ByteFunction {
    /// The offset into the instruction buffer where this function starts.
    pub offset: u32,
    /// The amount of instructions part of this function.
    pub size: u32,
    /// The amount of register this function uses.
    pub registers: u8,
    // A list of index of capture upvalues in the function scope
    pub upvalues: Box<[Upvalue]>,
}

/// A generic set of instructions, functions and constants.
#[derive(Debug)]
pub struct ByteCode {
    /// Constants used in this bytecode set.
    pub constants: Box<[Value]>,
    /// The functions defined in this bytecode, the entry function is always the first one.
    pub functions: Box<[ByteFunction]>,
    //// All instructions beloning to all functions defined in the bytecode.
    pub instructions: Box<[Instruction]>,
}

impl ByteCode {
    /// Makes sure that bytecode is valid and returns a struct signifying that the bytecode has
    /// been validated.
    pub fn validate(self) -> Result<ValidByteCode, (Self, ValidationError)> {
        unsafe {
            match self.is_valid() {
                Ok(()) => Ok(self.assume_valid()),
                Err(x) => Err((self, x)),
            }
        }
    }

    pub fn is_valid(&self) -> Result<(), ValidationError> {
        todo!()
    }

    pub unsafe fn assume_valid(self) -> ValidByteCode {
        ValidByteCode(self)
    }
}

/// Byte code which has been validated to ensure that the assumptions the vm makes about the
/// bytecode hold hold.
pub struct ValidByteCode(ByteCode);

impl ValidByteCode {
    pub fn into_inner(self) -> ByteCode {
        self.0
    }
}

unsafe impl Trace for ByteCode {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: gc::Ctx) {
        self.constants.iter().for_each(|x| x.trace(ctx))
    }
}

impl fmt::Display for ByteCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "> CONSTANTS")?;
        for (idx, c) in self.constants.iter().enumerate() {
            writeln!(f, "{:>4}: {:?}", idx, c)?;
        }
        writeln!(f)?;
        writeln!(f, "> FUNCTIONS")?;
        for (idx, func) in self.functions.iter().enumerate() {
            writeln!(
                f,
                "= FUNC:{:<4} registers:{:<2} instructions:{} upvalues:{}",
                idx,
                func.registers,
                func.size,
                func.upvalues.len()
            )?;

            if !func.upvalues.is_empty() {
                writeln!(f, "- UPVALUES")?;
            }

            for (idx, u) in func.upvalues.iter().copied().enumerate() {
                write!(f, "{:>4}: ", idx)?;
                match u {
                    Upvalue::Local(x) => {
                        writeln!(f, "LOCAL 0x{:x}", x)?;
                    }
                    Upvalue::Parent(x) => {
                        writeln!(f, "PARENT 0x{:x}", x)?;
                    }
                }
            }

            let start = func.offset as usize;
            let end = start + func.size as usize;

            writeln!(f, "- INSTRUCTIONS")?;

            for (idx, instr) in self.instructions[start..end].iter().enumerate() {
                write!(f, "{:>4}: ", idx)?;
                write!(f, "{}", instr)?;
                writeln!(f)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

define_instructions! {
    pub enum Instruction {
        /// Load a constant value.
        LoadConst{dst: u8,cons: u16},
        /// Load the global object.
        LoadGlobal{dst: u8},
        /// Load the execution environment of a certain depth.
        LoadEnv{dst: u8,depth: u16},
        /// Load a function from the current module.
        LoadFunction{ dst: u8, func: u16},

        Move{ dst: u8, src: u8},

        CreateObject{ dst: u8},

        IndexAssign{obj: u8,key: u8, val:u8},
        Index{dst: u8,obj: u8, key:u8},

        Upvalue{dst: u8, slot: u16},
        UpvalueAssign{src: u8, slot: u16},

        EnvAssign{env: u8, val:u8, key:u8},
        EnvIndex{dst: u8, env:u8, key:u8},

        In{dst: u8, left:u8, righ:u8},
        InstanceOf{dst: u8, left:u8, righ:u8},

        Add{dst: u8, left: u8, righ:u8},
        Sub{dst: u8, left: u8, righ:u8},
        Mul{dst: u8, left: u8, righ:u8},
        Div{dst: u8, left: u8, righ:u8},
        Pow{dst: u8, left: u8, righ:u8},
        Mod{dst: u8, left: u8, righ:u8},

        BitwiseAnd{dst: u8, left: u8, righ:u8},
        BitwiseOr{dst: u8, left: u8, righ:u8},
        BitwiseXor{dst: u8, left: u8, righ:u8},

        ShiftLeft{dst: u8, left: u8, righ:u8},
        ShiftRight{dst: u8, left: u8, righ:u8},
        ShiftUnsigned{dst: u8, left: u8, righ:u8},


        Equal{dst: u8, left: u8, righ:u8},
        SEqual{dst: u8, left: u8, righ:u8},
        NotEqual{dst: u8, left: u8, righ:u8},
        SNotEqual{dst: u8, left: u8, righ:u8},

        Greater{dst: u8, left: u8, righ:u8},
        GreaterEq{dst: u8, left: u8, righ:u8},
        Less{dst: u8, left: u8, righ:u8},
        LessEq{dst: u8, left: u8, righ:u8},

        Negative{dst: u8, op: u8},

        ToNumber{dst: u8, op: u8},
        ToBool{dst: u8, op: u8},

        IsNullish{dst: u8, op: u8},
        Not{ dst: u8, src: u8},

        JumpTrue{cond: u8, tgt:i16},
        JumpFalse{cond: u8, tgt:i16},
        Jump{tgt:i16},

        Push{ src: u8},

        Call{dst:u8, func:u8},

        Return{ret: u8},
        ReturnUndefined{ _ignore:()},
    }
}
