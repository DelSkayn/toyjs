//! Module containing the instruction information
//!
//! Instructions come in two different formats.
//! 1. The simple to use enum representation in [`Instruction`]
//! 2. The dense format used in the runtime in the form of a series of bytes [`InstructionBuffer`]

#[macro_use]
mod macros;
mod buffer;
pub use buffer::{InstructionBuffer, InstructionReader};

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

        IndexAssign{obj: u8,key: u8, value:u8},
        Index{dst: u8,obj: u8, key:u8},

        EnvAssign{env: u8, val:u8, key:u8},
        EnvIndex{dst: u8, env:u8, key:u8},

        Add{dst: u8, left: u8, righ:u8},
        Sub{dst: u8, left: u8, righ:u8},
        Mul{dst: u8, left: u8, righ:u8},
        Div{dst: u8, left: u8, righ:u8},
        Pow{dst: u8, left: u8, righ:u8},
        Mod{dst: u8, left: u8, righ:u8},

        BinaryAnd{dst: u8, left: u8, righ:u8},
        BinaryOr{dst: u8, left: u8, righ:u8},
        BinaryXor{dst: u8, left: u8, righ:u8},

        ShiftLeft{dst: u8, left: u8, righ:u8},
        ShiftRight{dst: u8, left: u8, righ:u8},
        ShiftUnsigned{dst: u8, left: u8, righ:u8},


        Equal{dst: u8, left: u8, righ:u8},
        StrictEqual{dst: u8, left: u8, righ:u8},
        NotEqual{dst: u8, left: u8, righ:u8},
        StrictNotEqual{dst: u8, left: u8, righ:u8},

        Greater{dst: u8, left: u8, righ:u8},
        GreaterEqual{dst: u8, left: u8, righ:u8},
        Lesser{dst: u8, left: u8, righ:u8},
        LesserEqual{dst: u8, left: u8, righ:u8},

        Negative{dst: u8, op: u16},

        ToNumber{dst: u8, op: u16},
        ToBool{dst: u8, op: u16},

        IsNullish{dst: u8, op: u16},

        JumpTrue{cond: u8, tgt:i16},
        JumpFalse{cond: u8, tgt:i16},
        Jump{null: u8, tgt:i16},

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
