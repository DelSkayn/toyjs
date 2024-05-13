use std::fmt;

use bytemuck::{Contiguous, Pod, TransparentWrapper, Zeroable};
use common::span::Span;
use dreck::Trace;

mod display;
mod instructions;
pub mod util;
pub use instructions::*;
mod r#macro;
mod reader;
pub use reader::{BcValid, ByteCodeReader, SafeByteCodeReader};
//mod writer;
//pub use writer::{ByteCodeWriter, WriteOffset};

/// A module with specific bytecode and VM specific limitations.
pub mod limits {
    /// The maximum number of registers a function frame can have.
    ///
    /// Limit is a result from how registers are addressed in thye bytecode.
    /// Every register is addressed via a i8 where all positive values can be a register.
    pub const MAX_REGISTERS: usize = i32::MAX as usize;
    pub const MAX_DIRECT_REGISTERS: usize = i8::MAX as usize;
    /// The maximum number of arguments that can be directly addressed from an instruction.
    ///
    /// Limit is a result from how arguments are addressed in the bytecode.
    /// A set of arguments is addressable via negative register values.
    /// Register -1 and -2 are the info for the stack frame and the current function object so
    /// these are excluded.
    pub const MAX_ADDRESSABLE_ARGUMENTS: usize = -(i8::MIN as isize) as usize - 2;

    /// The maximum number of values an function can be called with.
    ///
    /// Limit is a result from how the stack stores stack information.
    pub const MAX_ARGUMENTS: usize = u32::MAX as usize - 2;

    /// The maximum bytecode size of a single function.
    ///
    /// If a function is bigger than this no single jump will be able to jump far enough if
    /// required.
    pub const MAX_BYTECODE_SIZE: usize = u32::MAX as usize;
}

pub struct ByteCode {
    pub functions: Box<[Function]>,
    pub strings: Box<[common::string::String]>,
    pub instructions: Box<[u8]>,
}

unsafe impl<'own> Trace<'own> for ByteCode {
    type Gc<'r> = ByteCode;

    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        false
    }

    fn trace(&self, _marker: dreck::Marker<'own, '_>) {}
}

/// A newtype for a register index.
///
/// An instruction can directly access upto 127 register and 126 arguments.
#[derive(Clone, Copy, Eq, PartialEq, Debug, Pod, TransparentWrapper, Zeroable, Ord, PartialOrd)]
#[repr(transparent)]
pub struct Reg(pub i8);

impl Reg {
    pub const MAX: i8 = i8::MAX;
    pub const MIN: i8 = i8::MIN;
    /// A register to use when a temporary register is required for writing.
    /// Should always be patched later, not emited.
    pub const fn tmp() -> Self {
        Reg(-2)
    }

    /// Returns the register index of the place where the `this` value is stored.
    pub const fn this_reg() -> Self {
        Reg(-1)
    }

    /// Returns the register for the current function being executed.
    pub const fn function_reg() -> Self {
        Reg(-2)
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "r{}", self.0)
    }
}

/// Error that happens when trying to create a register for an stack value outside of i8 range.
#[derive(Debug, Clone, Copy)]
pub struct RegOutOfRange;

impl fmt::Display for RegOutOfRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Tried to create register from out of range offset")
    }
}

impl TryFrom<LongReg> for Reg {
    type Error = RegOutOfRange;

    fn try_from(value: LongReg) -> Result<Self, Self::Error> {
        i8::try_from(value.0).map(Reg).map_err(|_| RegOutOfRange)
    }
}

/// A newtype for a register of a far index.
///
/// An instruction can directly access upto 127 register and 126 arguments.
/// When an instruction needs to read from a further address this register is used.
#[derive(Clone, Copy, Eq, PartialEq, Debug, Pod, TransparentWrapper, Zeroable)]
#[repr(transparent)]
pub struct LongReg(pub i32);

impl LongReg {
    /// Returns the register index of the place where the `this` value is stored.
    pub const fn this_reg() -> Self {
        LongReg(-1)
    }

    /// Returns the register for the current function being executed.
    pub const fn function_reg() -> Self {
        LongReg(-2)
    }
}

impl From<Reg> for LongReg {
    fn from(value: Reg) -> Self {
        LongReg(value.0 as i32)
    }
}

impl fmt::Display for LongReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fr{}", self.0)
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

#[derive(Clone, Copy, Eq, PartialEq, Debug, Pod, TransparentWrapper, Zeroable)]
#[repr(transparent)]
pub struct LongOffset(pub i32);

impl fmt::Display for LongOffset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A newtype for a primitive value.
#[derive(Clone, Copy, Eq, PartialEq, Debug, Pod, TransparentWrapper, Zeroable, Ord, PartialOrd)]
#[repr(transparent)]
pub struct Primitive(pub u8);

impl Primitive {
    pub const TRUE: Self = Self::t();

    pub const fn empty() -> Self {
        Primitive(0)
    }

    pub const fn null() -> Self {
        Primitive(2)
    }

    pub const fn deleted() -> Self {
        Primitive(5)
    }

    pub const fn t() -> Self {
        Primitive(6)
    }

    pub const fn f() -> Self {
        Primitive(7)
    }

    pub const fn undefined() -> Self {
        Primitive(10)
    }
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            0 => write!(f, "Empty"),
            2 => write!(f, "Null"),
            5 => write!(f, "Deleted"),
            6 => write!(f, "True"),
            7 => write!(f, "False"),
            10 => write!(f, "Undefined"),
            x => panic!("invalid primitive {x}"),
        }
    }
}

/// A newtype for a string constant ids.
#[derive(Clone, Copy, Eq, PartialEq, Debug, Pod, TransparentWrapper, Zeroable, Hash)]
#[repr(transparent)]
pub struct StringId(pub u16);

impl fmt::Display for StringId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "s{}", self.0)
    }
}

/*
pub struct String {
    // offset into the string buffer.
    pub offset: u32,
    // the amount of codepoints in this string.
    pub len: u32,
    pub kind: StringKind,
}

pub enum StringKind {
    Ascii,
    Utf16,
}
*/

/// A newtype for a bytecode function constants.
#[derive(Clone, Copy, Eq, PartialEq, Debug, Pod, TransparentWrapper, Zeroable)]
#[repr(transparent)]
pub struct UpvalueId(pub u16);

impl fmt::Display for UpvalueId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "u{}", self.0)
    }
}

/// A newtype for a bytecode function constants.
#[derive(Clone, Copy, Eq, PartialEq, Debug, Pod, TransparentWrapper, Zeroable)]
#[repr(transparent)]
pub struct FunctionId(pub u32);

impl FunctionId {
    /// Returns the id of an entry function.
    pub fn entry() -> FunctionId {
        FunctionId(0)
    }
}

impl fmt::Display for FunctionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "f{}", self.0)
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Contiguous)]
pub enum Builtin {
    FunctionApply = 0,
    ArraySpread = 1,
    ObjectSpread = 2,
    ObjectAssign = 3,
    DefineGetter = 4,
    DefineSetter = 5,
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Builtin {
    pub fn to_byte(self) -> BuiltinByte {
        BuiltinByte(self as u8)
    }
}

/// A newtype for a bytecode function constants.
#[derive(Clone, Copy, Eq, PartialEq, Debug, Pod, TransparentWrapper, Zeroable)]
#[repr(transparent)]
pub struct BuiltinByte(pub u8);

impl fmt::Display for BuiltinByte {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Builtin::from_integer(self.0).unwrap())
    }
}

pub struct Function {
    /// offset into the instruction buffer.
    pub offset: u32,
    /// the amount of bytes in this function.
    pub len: u32,
    /// Information used to reconstruct variable locations for functions with direct eval.
    pub reflect_info: Option<()>,
    /// The span of the function in the source code.
    pub span: Span,
    /// The amount of upvalues this function uses.
    pub upvalues: u32,
    /// The amount of upvalues this function uses.
    pub registers: u32,
}
