mod constant;
pub use constant::{Constant, Null, Undefined};
mod builder;
pub use builder::SsaBuilder;

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub struct ConstantId(pub u32);

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub struct InstrVar(u32);

impl InstrVar {
    pub fn null() -> Self {
        InstrVar(u32::max_value())
    }

    pub fn as_u32(self) -> u32 {
        assert!(self != Self::null());
        self.0
    }
}

impl From<u32> for InstrVar {
    fn from(v: u32) -> Self {
        assert!(v != u32::max_value());
        InstrVar(v)
    }
}

impl From<SsaVar> for InstrVar {
    fn from(v: SsaVar) -> Self {
        InstrVar(v.0)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Postive,
    Negative,
    Not,
    BinaryNot,
    New,
    Delete,
    Typeof,
    Void,
    ToBool,
    IsNullish,
    AddOne,
    SubtractOne,
}

#[derive(Clone, Copy, Debug)]
pub enum BinOp {
    InstanceOf,
    In,
    And,
    Or,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    ShiftLeft,
    ShiftRight,
    ShiftRightUnsigned,
    Dot,
    Equal,
    StrictEqual,
    NotEqual,
    StrictNotEqual,
}

#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    LoadGlobal,
    ObjectSet {
        object: InstrVar,
        key: InstrVar,
        value: InstrVar,
    },
    ObjectGet {
        object: InstrVar,
        key: InstrVar,
    },
    Move {
        operand: InstrVar,
    },
    /// Do a unary operations on an operand.
    Unary {
        kind: UnaryOp,
        operand: InstrVar,
    },
    /// Do a binary operations on the 2 operands.
    Binary {
        kind: BinOp,
        left: InstrVar,
        right: InstrVar,
    },
    /// Jump to target if the value at condition is thruthy,
    /// if negative is true jump if the condition is falsey
    CondJump {
        negative: bool,
        condition: InstrVar,
        target: InstrVar,
    },
    /// Jump to the target instruction
    Jump {
        target: InstrVar,
    },
    /// load a constant
    LoadConstant {
        constant: ConstantId,
    },
    /// Declare to ssa instruction declarations to be the same instruction.
    Alias {
        left: InstrVar,
        right: InstrVar,
    },
    /// Return from the current stack the value
    /// Can be null in which case undefined is returned
    Return {
        value: InstrVar,
    },
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct SsaVar(u32);

#[derive(Debug)]
pub struct Ssa {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<Constant>,
}
