use fxhash::FxHashMap;

mod constant;
pub use constant::{Constant, Null, Undefined};

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub struct ConstantId(pub u32);

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub struct InstrVar(pub u32);

impl InstrVar {
    pub fn null() -> Self {
        InstrVar(u32::max_value())
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
    /// Do a unary operations on an operand.
    Unary { kind: UnaryOp, operand: InstrVar },
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
    Jump { target: InstrVar },
    /// load a constant
    LoadConstant { constant: ConstantId },
    /// Declare to ssa instruction declarations to be the same instruction.
    Alias { left: InstrVar, right: InstrVar },
    /// Return from the current stack the value
    /// Can be null in which case undefined is returned
    Return { value: InstrVar },
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct SsaVar(u32);

#[derive(Debug)]
pub struct Ssa {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<Constant>,
}

pub struct SsaBuilder {
    instructions: Vec<Instruction>,
    constants: Vec<Constant>,
    constant_location: FxHashMap<Constant, SsaVar>,
}

impl SsaBuilder {
    pub fn new() -> Self {
        SsaBuilder {
            instructions: Vec::new(),
            constants: Vec::new(),
            constant_location: FxHashMap::default(),
        }
    }

    pub fn get_mut(&mut self, var: SsaVar) -> &mut Instruction {
        &mut self.instructions[var.0 as usize]
    }

    pub fn next_id(&self) -> SsaVar {
        SsaVar(self.instructions.len() as u32)
    }

    pub fn push_instruction(&mut self, instr: Instruction) -> SsaVar {
        assert!(
            self.instructions.len() < (u32::max_value() - 2) as usize,
            "to many instructions!"
        );
        let id = self.instructions.len() as u32;
        self.instructions.push(instr);
        SsaVar(id)
    }

    pub fn patch_jump_target(&mut self, instr: SsaVar, target: InstrVar) {
        let t = target;
        match self.instructions[instr.0 as usize] {
            Instruction::Jump { ref mut target } => *target = t,
            Instruction::CondJump {
                negative: _,
                condition: _,
                ref mut target,
            } => *target = t,
            _ => panic!("ssa instruction to be patched is not a jump"),
        }
    }

    pub fn load_constant<T: Into<Constant>>(&mut self, t: T) -> SsaVar {
        self.load_constant_inner(t.into())
    }

    fn load_constant_inner(&mut self, c: Constant) -> SsaVar {
        if let Some(x) = self.constant_location.get(&c) {
            return *x;
        }
        assert!(
            self.constants.len() < (u32::max_value() - 2) as usize,
            "to many constants!"
        );
        let const_id = self.constants.len();
        self.constants.push(c.clone());
        let res = self.push_instruction(Instruction::LoadConstant {
            constant: ConstantId(const_id as u32),
        });
        self.constant_location.insert(c.clone(), res);
        res
    }

    pub fn build(self) -> Ssa {
        Ssa {
            instructions: self.instructions,
            constants: self.constants,
        }
    }
}
