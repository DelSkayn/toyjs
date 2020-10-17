//use crate::constant::ConstantId;
use crate::constants::ConstantId;
use bumpalo::{collections::Vec, Bump};
use common::{collections::HashMap, index::Index, newtype_index, newtype_vec};

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
pub struct SsaVec<'alloc> {
    instructions: Vec<'alloc, Ssa>,
    envs: HashMap<u32, SsaId>,
    global: Option<SsaId>,
}

newtype_vec!(
    struct SsaVec<'alloc,>.instructions[SsaId] -> Ssa
);

impl<'alloc> SsaVec<'alloc> {
    pub fn new_in(bump: &'alloc Bump) -> Self {
        SsaVec {
            instructions: Vec::new_in(bump),
            envs: HashMap::default(),
            global: None,
        }
    }

    pub fn push_env(&mut self, depth: u32) {
        assert!(self.envs.len() == self.instructions.len());
        if !self.envs.contains_key(&dbg!(depth)) {
            let id = self.insert(Ssa::GetEnvironment { depth });
            self.envs.insert(depth, id);
        }
    }

    pub fn push_global(&mut self) {
        assert!(self.global.is_none());
        self.global = Some(self.insert(Ssa::GetGlobal));
    }

    pub fn insert(&mut self, instruction: Ssa) -> SsaId {
        let id = SsaId::from(self.instructions.len());
        self.instructions.push(instruction);
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

    pub fn environment(&self, depth: u32) -> SsaId {
        self.envs.get(&dbg!(depth)).copied().unwrap()
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
        depth: u32,
    },
    IndexEnvironment {
        env: SsaId,
        slot: u32,
    },
    AssignEnvironment {
        value: SsaId,
        env: SsaId,
        slot: u32,
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
