use std::fmt;

use bytemuck::{Pod, TransparentWrapper, Zeroable};

mod r#macro;
mod reader;
pub use reader::{BcValid, ByteCodeReader, SafeByteCodeReader};

pub struct ByteCode {
    pub instructions: Box<[u8]>,
}

/// A newtype for a register index.
#[derive(Clone, Copy, Eq, PartialEq, Debug, Pod, TransparentWrapper, Zeroable)]
#[repr(transparent)]
pub struct Reg(pub u8);

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "r{}", self.0)
    }
}

/// A newtype for a instruction offset for jumps.
#[derive(Clone, Copy, Eq, PartialEq, Debug, Pod, TransparentWrapper, Zeroable)]
#[repr(transparent)]
pub struct Offset(pub i16);

impl fmt::Display for Offset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

instruction! {
    /// Load the i8 value in imm into dst
    Loadi8{ dst: Reg, imm: i8 },
    /// Load the i32 value in imm into dst
    Loadi32{ dst: Reg, imm: i32 },
    /// Load the f64 value in imm into dst
    Loadf64{ dst: Reg, imm: f64 },
    /// Load the bool value in imm into dst
    LoadBool{ dst: Reg, imm: u8 },
    /// Load the undefined value into dst
    LoadUndefined{ dst: Reg},
    /// Load the string with id `const` into dst
    LoadString{ dst: Reg, cons: u16 },

    /// Load the current `this` value
    LoadThis{ dst: Reg },
    /// Load the current `new.target` value
    LoadTarget{ dst: Reg },

    /// Copy the value from src into dst.
    Move{ dst: Reg , src: Reg},

    /// Add left to right and store the result into dst.
    Add{ dst: Reg, left: Reg, right: Reg},
    /// Subtrace left form right and store the result into dst.
    Sub{ dst: Reg, left: Reg, right: Reg},
    /// Multiply left by right and store the result into dst.
    Mul{ dst: Reg, left: Reg, right: Reg},
    /// Divide left by right and store the result into dst.
    Div{ dst: Reg, left: Reg, right: Reg},
    /// Modulo left by right and store the result into dst.
    Mod{ dst: Reg, left: Reg, right: Reg},
    /// Raise left by rigth store the result into dst.
    Pow{ dst: Reg, left: Reg, right: Reg},

    /// Shift left by rigth and store the result into dst
    ShiftL{ dst: Reg, left: Reg, right: Reg},
    /// Shift right by rigth and store the result into dst
    ShiftR{ dst: Reg, left: Reg, right: Reg},
    /// Shift right unsighned by rigth and store the result into dst
    ShiftRU{ dst: Reg, left: Reg, right: Reg},

    /// Calculate if left and right are equal and store the result in dst.
    Equal{ dst: Reg, left: Reg, right: Reg},
    /// Calculate if left and right are strictly equal and store the result in dst.
    SEqual{ dst: Reg, left: Reg, right: Reg},
    /// Calculate if left and right are not equal and store the result in dst.
    NotEqual{ dst: Reg, left: Reg, right: Reg},
    /// Calculate if left and right are strictly not equal and store the result in dst.
    SNotEqual{ dst: Reg, left: Reg, right: Reg},

    /// Calculate if left is greater then right and store the result in dst.
    Greater{ dst: Reg, left: Reg, right: Reg},
    /// Calculate if left is greater then  or equal to right and store the result in dst.
    GreaterEq{ dst: Reg, left: Reg, right: Reg},
    /// Calculate if left is less then right and store the result in dst.
    Less{ dst: Reg, left: Reg, right: Reg},
    /// Calculate if left is less then or equal to right and store the result in dst.
    LessEq{ dst: Reg, left: Reg, right: Reg},

    /// Calculate bitwise and of left and right and store the result in dst.
    BitAnd{ dst: Reg, left: Reg, right: Reg},
    /// Calculate bitwise or of left and right and store the result in dst.
    BitOr{ dst: Reg, left: Reg, right: Reg},
    /// Calculate bitwise exclusive or of left and right and store the result in dst.
    BitXor{ dst: Reg, left: Reg, right: Reg},
    /// Calculate bitwise not of src and store the result in dst.
    BitNot{ dst: Reg, src: Reg},

    /// Calculate negative of src and store the result in dst.
    Neg{ dst: Reg, src: Reg},
    /// Coerce src into a number and store the result in dst.
    ToNum{ dst: Reg, src: Reg},
    /// Calculate if src is falsish and store the result in dst.
    Not{ dst: Reg, src: Reg},

    /// Jump to instruction with offset dst.
    Jump{ dst: Offset},
    /// Jump to instruction with offset dst if cond is trueish.
    JumpTrue{ cond: Reg, dst: Offset},
    /// Jump to instruction with offset dst if cond is falsish.
    JumpFalse{ cond: Reg, dst: Offset},

    /// Throw the error value in src
    Throw{ src: Reg },
    ///  Retrieve the thrown error value and store it in dst
    Catch{ dst: Reg },

    /// Return from the current function with a undefined value.
    RetUndefind{},
    /// Return from the current function with a the value in the src register.
    Ret{src: Reg},

    /// Call the function in func and store its result in ret..
    Call{func: Reg, ret: Reg},
}
