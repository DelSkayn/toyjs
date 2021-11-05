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
mod buffer;
use std::{error::Error, fmt};

pub use buffer::{InstructionBuffer, InstructionReader};

#[derive(Debug)]
pub enum ValidationError {
    FunctionOffsetInvalid {
        /// Which function
        function: usize,
        /// The invalid offset
        offset: usize,
        /// The size of the buffer.
        buffer_size: usize,
    },
    InvalidOpcode {
        /// Which function
        function: usize,
        /// The offset of the invalid opcode in the instruction buffer.
        offset: usize,
        /// The invalid opcode
        opcode: u8,
    },
}

impl Error for ValidationError {}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::FunctionOffsetInvalid {
                function,
                offset,
                buffer_size,
            } => {
                write!(f,"Function `{}` had an invalid offset into its instruction buffer. The offset was `{}` while the instruction buffer had a size of `{}`",function,offset,buffer_size)?;
            }
            _ => todo!(),
        }
        Ok(())
    }
}

use crate::{
    gc::{self, Trace},
    JSValue,
};

#[derive(Clone, Copy)]
pub struct ByteFunction {
    /// The offset into the instruction buffer where this function starts.
    pub offset: usize,
    /// The amount of instructions part of this function.
    pub size: usize,
    /// The amount of register this function uses.
    pub registers: u8,
}

/// A generic set of instructions, functions and constants.
pub struct ByteCode {
    /// Constants used in this bytecode set.
    pub constants: Box<[JSValue]>,
    /// The functions defined in this bytecode, the entry function is always the first one.
    pub functions: Box<[ByteFunction]>,
    //// All instructions beloning to all functions defined in the bytecode.
    pub instructions: InstructionBuffer,
}

impl ByteCode {
    /// Makes sure that bytecode is valid and returns a struct signifying that the bytecode has
    /// been validated.
    pub fn validate(self) -> Result<ValidByteCode, (Self, ValidationError)> {
        unsafe {
            match self.is_valid() {
                None => Ok(ValidByteCode::assume_valid(self)),
                Some(x) => Err((self, x)),
            }
        }
    }

    pub fn is_valid(&self) -> Option<ValidationError> {
        let mut _l_values = Vec::<usize>::new();

        for (f_idx, f) in self.functions.iter().enumerate() {
            if f.offset >= self.instructions.size() {
                return Some(ValidationError::FunctionOffsetInvalid {
                    function: f_idx,
                    offset: f.offset,
                    buffer_size: self.instructions.size(),
                });
            }
            let _reader = InstructionReader::new(&self.instructions, f.offset, f.size);
        }
        todo!();
    }
}

/// Byte code which has been validated to ensure that the assumptions the vm makes about the
/// bytecode hold hold.
pub struct ValidByteCode(ByteCode);

impl ValidByteCode {
    pub unsafe fn assume_valid(bc: ByteCode) -> ValidByteCode {
        ValidByteCode(bc)
    }

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
        writeln!(f, "> INSTRUCTIONS")?;
        for (idx, func) in self.functions.iter().enumerate() {
            writeln!(
                f,
                "= FUNC:{:<4} registers:{:<2} instructions:{}",
                idx, func.registers, func.size
            )?;

            let mut reader = InstructionReader::new(&self.instructions, func.offset, func.size);
            let mut idx = 0;

            while !reader.at_end() {
                write!(f, "{:>4}: ", idx)?;
                Instruction::format_byte_instruction(f, &mut reader)?;
                writeln!(f)?;
                idx += 1;
            }
        }
        Ok(())
    }
}

/// Possible instruction types
#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub enum InstructionKind {
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
    /// An instruction in the form of
    /// ```
    /// 0  2  4  6  8 10 12 14 16 18 20 22 24 26 28 30 32                                               64
    /// +-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+
    /// |  OP CODE  |   REG A   |         REG D         |                     REG L                     |
    /// +-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+
    /// ```
    L,
}

define_instructions! {
    pub enum Instruction {
        /// Load a constant value.
        LoadConst{dst: u8,cons: u16},
        /// Load the global object.
        LoadGlobal{dst: u8, null: u16},
        /// Load the execution environment of a certain depth.
        LoadEnv{dst: u8,depth: u16},
        /// Load a function from the current module.
        LoadFunction{ dst: u8, func: u16},

        Move{ dst: u8, src: u16 },

        CreateObject{ dst: u8, null: u16},

        IndexAssign{obj: u8,key: u8, val:u8},
        Index{dst: u8,obj: u8, key:u8},

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

        Negative{dst: u8, op: u16},

        ToNumber{dst: u8, op: u16},
        ToBool{dst: u8, op: u16},

        IsNullish{dst: u8, op: u16},
        Not{ dst: u8, src: u16},

        JumpTrue{cond: u8, tgt:i16},
        JumpFalse{cond: u8, tgt:i16},
        Jump{null: u8, tgt:i16},

        SetArg{ tgt: u8, src: u16 },

        Call{dst:u8, func:u8, num:u8},

        Return{ret: u8,null: u16},
        ReturnUndefined{nul0: u8,nul1: u16},

        /// Load a constant value with an index larger then u16.
        LoadConstL{dst: u8, null: u16,cons: u32},

        LoadFunctionL{ dst: u8, null: u16,func: u32},

        JumpTrueL{cond: u8, null:u16, tgt:i32},
        JumpFalseL{cond: u8, null:u16, tgt:i32},
        JumpL{nul0: u8, nul1:u16, tgt:i32},
    }
}
