use bytemuck::{Pod, TransparentWrapper, Zeroable};
use common::span::Span;
use dreck::Trace;
use std::fmt;

mod instructions;
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
    pub const MAX_REGISTERS: usize = i8::MAX as usize;
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
    pub strings: Box<[String]>,
    pub string_buffer: Box<[u16]>,
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
#[derive(Clone, Copy, Eq, PartialEq, Debug, Pod, TransparentWrapper, Zeroable)]
#[repr(transparent)]
pub struct Reg(pub i8);

impl Reg {
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

/// A newtype for a string constant ids.
#[derive(Clone, Copy, Eq, PartialEq, Debug, Pod, TransparentWrapper, Zeroable)]
#[repr(transparent)]
pub struct StringId(u16);

impl fmt::Display for StringId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "s{}", self.0)
    }
}

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

/// A newtype for a bytecode function constants.
#[derive(Clone, Copy, Eq, PartialEq, Debug, Pod, TransparentWrapper, Zeroable)]
#[repr(transparent)]
pub struct FunctionId(pub u16);

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

pub struct Function {
    // offset into the instruction buffer.
    pub offset: u32,
    // the amount of bytes in this function.
    pub len: u32,
    // Information used to reconstruct variable locations for functions with direct eval.
    pub reflect_info: Option<()>,
    // The span of the function in the source code.
    pub span: Span,
}

impl fmt::Display for ByteCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, " - INSTRUCTIONS -")?;
        let mut reader = SafeByteCodeReader::from_bc(&self.instructions);
        while let Some(instr) = reader.read_instruction() {
            writeln!(f, "{}", instr)?;
        }

        Ok(())
    }
}
