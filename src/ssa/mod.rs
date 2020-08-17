mod constant;
pub use constant::{Constant, Null, Undefined};
mod builder;
use crate::{
    interner::StringId,
    util::{Index, Integer},
};
pub use builder::{
    BindingType, Expr, SsaBuilder, SsaFactory, VariableId, VariableTable, Variables,
};

shrinkwrap_index!(ConstantId);
shrinkwrap_index!(SsaId, OptionSsaId);

impl OptionSsaId {
    pub const fn null() -> Self {
        OptionSsaId::none()
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
    CreateEnv,
    GetVariable {
        variable: VariableId,
    },
    SetVariable {
        variable: VariableId,
        value: SsaId,
    },
    ObjectSet {
        object: SsaId,
        key: SsaId,
        value: SsaId,
    },
    ObjectGet {
        object: SsaId,
        key: SsaId,
    },
    Move {
        operand: SsaId,
    },
    /// Do a unary operations on an operand.
    Unary {
        kind: UnaryOp,
        operand: SsaId,
    },
    /// Do a binary operations on the 2 operands.
    Binary {
        kind: BinOp,
        left: SsaId,
        right: SsaId,
    },
    /// Jump to target if the value at condition is thruthy,
    /// if negative is true jump if the condition is falsey
    CondJump {
        thruthy: bool,
        condition: SsaId,
        // Should only be null during building
        target: OptionSsaId,
    },
    /// Jump to the target instruction
    Jump {
        // Should only be null during building
        target: OptionSsaId,
    },
    /// load a constant
    LoadConstant {
        constant: ConstantId,
    },
    /// Declare to ssa instruction declarations to be the same instruction.
    Alias {
        left: SsaId,
        right: SsaId,
    },
    /// Return from the current stack the value
    /// Can be null in which case undefined is returned
    Return {
        value: OptionSsaId,
    },
}

#[derive(Debug)]
pub struct SsaFunction {
    pub instructions: Vec<Instruction>,
    pub name: Option<StringId>,
    pub strict: bool,
}

impl SsaFunction {
    fn new(name: Option<StringId>) -> SsaFunction {
        SsaFunction {
            instructions: vec![Instruction::LoadGlobal, Instruction::CreateEnv],
            name,
            strict: false,
        }
    }
}

#[derive(Debug)]
pub struct Ssa {
    pub functions: Vec<SsaFunction>,
    pub variables: Variables,
    pub constants: Vec<Constant>,
}
