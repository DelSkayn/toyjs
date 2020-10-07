//use crate::constant::ConstantId;
use crate::constants::ConstantId;
use bumpalo::{collections::Vec, Bump};
use common::{index::Index, newtype_index, newtype_vec};
newtype_index! ( #[derive(Ord,PartialOrd)]
    pub struct SsaId
);

impl SsaId {
    pub fn prev(self) -> Self {
        assert!(self.0 > 0);
        SsaId(self.0 - 1)
    }
    pub fn next(self) -> Self {
        assert!(self.0 < Index::MAX);
        SsaId(self.0 + 1)
    }
}

#[derive(Debug)]
pub struct SsaVec<'alloc>(pub Vec<'alloc, Ssa>);

newtype_vec!(
    struct SsaVec<'alloc,>[SsaId] -> Ssa
);

impl<'alloc> SsaVec<'alloc> {
    pub fn new_in(bump: &'alloc Bump) -> Self {
        SsaVec(bumpalo::vec![in bump; Ssa::GetGlobal, Ssa::CreateEnvironment])
    }

    pub fn insert(&mut self, instruction: Ssa) -> SsaId {
        let id = SsaId::from(self.0.len());
        self.0.push(instruction);
        id
    }

    pub fn patch_jump(&mut self, jump: SsaId, target: SsaId) {
        match self[jump] {
            Ssa::ConditionalJump {
                ref mut to,
                condition: _,
            }
            | Ssa::Jump { ref mut to } => {
                *to = Some(target);
            }
            _ => panic!("patch instruction not a jump instruction"),
        }
    }

    pub fn global(&self) -> SsaId {
        SsaId::from(0u32)
    }

    pub fn environment(&self) -> SsaId {
        SsaId::from(1u32)
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum BinaryOperation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Exponentiate,
    Equal,
    NotEqual,
    StrictEqual,
    StrictNotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    LeftShift,
    RightShift,
    UnsignedRightShift,
    InstanceOf,
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum UnaryOperation {
    Not,
    BinaryNot,
    ToNumber,
    Negative,
    Typeof,
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum Ssa {
    GetGlobal,
    CreateEnvironment,
    GetEnvironment {
        depth: u16,
    },
    IndexEnvironment {
        env: SsaId,
        slot: u16,
    },
    AssignEnvironment {
        value: SsaId,
        env: SsaId,
        slot: u16,
    },
    CreateObject,
    Index {
        object: SsaId,
        key: SsaId,
    },
    Assign {
        object: SsaId,
        key: SsaId,
        value: SsaId,
    },
    ConditionalJump {
        condition: SsaId,
        to: Option<SsaId>,
    },
    Jump {
        to: Option<SsaId>,
    },
    LoadConstant {
        constant: ConstantId,
    },
    Binary {
        op: BinaryOperation,
        left: SsaId,
        right: SsaId,
    },
    Unary {
        op: UnaryOperation,
        operand: SsaId,
    },
    Return {
        expr: Option<SsaId>,
    },
    Alias {
        left: SsaId,
        right: SsaId,
    },
}
