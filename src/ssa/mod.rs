use fxhash::FxHashMap;

mod constant;
pub use constant::Constant;

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub enum Op {
    Addition,
    Subtraction,
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct InstrOp(u32);

impl InstrOp {
    pub fn null() -> Self {
        InstrOp(u32::max_value())
    }
}

impl From<u32> for InstrOp {
    fn from(v: u32) -> Self {
        assert!(v != u32::max_value());
        InstrOp(v)
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub enum InstrKind {
    Operation(Op),
    LoadConstant,
}

#[derive(Clone, Copy)]
pub struct Instruction {
    kind: InstrKind,
    left: InstrOp,
    right: InstrOp,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct SsaVar(u32);

pub struct Ssa {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<Constant>,
}

pub struct SsaBuilder {
    instructions: Vec<Instruction>,
    constants: Vec<Constant>,
    constant_location: FxHashMap<Constant, u32>,
}

impl SsaBuilder {
    pub fn new() -> Self {
        SsaBuilder {
            instructions: Vec::new(),
            constants: Vec::new(),
            constant_location: FxHashMap::default(),
        }
    }

    pub fn load_constant<T: Into<Constant>>(&mut self, t: T) -> SsaVar {
        self.load_constant_inner(t.into())
    }

    fn load_constant_inner(&mut self, c: Constant) -> SsaVar {
        let constant_id = self.place_constant(c);
        let id = self.instructions.len() as u32;
        self.instructions.push(Instruction {
            kind: InstrKind::LoadConstant,
            left: InstrOp::from(constant_id),
            right: InstrOp::null(),
        });
        SsaVar(id)
    }

    fn place_constant(&mut self, c: Constant) -> u32 {
        if let Some(x) = self.constant_location.get(&c) {
            return *x;
        }
        let clone = c.clone();
        assert!(
            self.constants.len() < (u32::max_value() - 2) as usize,
            "to many constants!"
        );
        let id = self.constants.len() as u32;
        self.constant_location.insert(clone, id);
        self.constants.push(c);
        id
    }
}
