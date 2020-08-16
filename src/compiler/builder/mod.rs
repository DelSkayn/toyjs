mod variable;
pub use variable::{VariableId, VariableTable};

#[derive(Clone, Copy)]
pub enum Expr {
    Index {
        object: SsaVar,
        index: SsaVar,
    },
    Ident {
        object: Option<SsaVar>,
        ident: StringId,
    },
    Expr(SsaVar),
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct SsaId(u32);

pub struct SsaRange {
    low: u32,
    high: u32,
}

impl SsaRange {
    pub fn first(self) -> SsaId {
        SsaId(self.low)
    }

    pub fn last(self) -> SsaId {
        SsaId(self.high - 1)
    }

    pub fn after(self) -> SsaId {
        SsaId(self.high)
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct VariableId(u32);

pub struct Builder {
    variable_table: VariableTable,

    variables: Vec<VariableId>,
    ssa: Vec<Ssa>,
}

impl Builder {
    pub fn new() -> Self {
        Builder {
            variable_table: VariableTable::new(),
            variables: Vec::new(),
            ssa: Vec::new(),
        }
    }

    fn push_instr(&mut self,instr: SsaInstr) -> SsaVar{
        let id = self.ssa.len();
        assert!(id < u32::MAX as usize, "to many instructions");
        self.ssa.push(id);
        SsaVar(id as u32)
    }

    fn expr_to_ssa_var(&mut self, expr: Expr) -> SsaVar{
        match expr{
            Expr::Expr(x) => x,
            Index{
                object,
                index,
            } => {
                let object = if let Some(obj) = object{
                    obj
                }else{
                    self.load_global()
                }
                self.
        }
    }


    pub fn load_global(&mut self) -> SsaVar{
        self.push_instr(SsaInstr::LoadGlobal)
    }

    pub fn constant(&mut self, constant: Constant) -> Expr {}

    pub fn binary_expression(&mut self, kind: BinOp, left: Expr, right: Expr) -> Expr {}

    pub fn unary_expression(&mut self, kind: UnaryOp, operand: Expr) -> Expr {}

    pub fn dot(&mut self, object: Option<Expr>, ident: StringId) -> Expr {
        Expr::Ident { object, ident }
    }

    pub fn index(&mut self, object: Expr, expr: Expr) -> Expr{
        Expr{
        }
    }

    pub fn next(&self) -> SsaId {
        let id = self.ssa.len();
        assert!(id < u32::MAX as usize);
        SsaId(id as u32)
    }
}
