use crate::{
    interner::StringId,
    ssa::{BinOp, Constant, ConstantId, InstrVar, Instruction, Ssa, SsaVar, UnaryOp},
};
use fxhash::FxHashMap;
use std::mem;
mod variable;
pub use variable::VariableId;
use variable::VariableTable;

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

pub struct ExprJumpContext {
    true_list: Vec<SsaVar>,
    false_list: Vec<SsaVar>,
}

pub struct StmtJumpContext {
    breaks: Vec<SsaVar>,
    continues: Vec<SsaVar>,
}

pub struct SsaFactory {
    variables: VariableTable,

    instructions: Vec<Instruction>,
    instruction_variables: Vec<VariableId>,

    constants: Vec<Constant>,
    constant_location: FxHashMap<Constant, usize>,

    true_list: Vec<SsaVar>,
    false_list: Vec<SsaVar>,
    breaks: Vec<SsaVar>,
    continues: Vec<SsaVar>,
}

impl Default for SsaFactory {
    fn default() -> Self {
        SsaFactory::new()
    }
}

impl SsaFactory {
    pub fn new() -> Self {
        let mut res = SsaFactory {
            variables: VariableTable::new(),

            instructions: Vec::new(),
            constants: Vec::new(),
            constant_location: FxHashMap::default(),
            true_list: Vec::new(),
            false_list: Vec::new(),
            breaks: Vec::new(),
            continues: Vec::new(),
        };
        res.push(VariableId::temporary(),Instruction::LoadGlobal);
        res.push(VariableId::temporary(),Instruction::CreateEnv);
    }

    pub fn next_id(&self) -> SsaVar {
        SsaVar(self.instructions.len() as u32)
    }


    pub fn push_temp(&mut self, instr: Instruction) -> SsaVar {
        assert!(
            self.instructions.len() < (u32::max_value() - 2) as usize,
            "to many instructions!"
        );
        let id = self.instructions.len() as u32;
        self.instructions.push(instr);
        self.instruction_variables.push(VariableId::temporary());
        SsaVar(id)
    }

    pub fn push(&mut self, variable: VariableId, instr: Instruction) -> SsaVar {
        assert!(
            self.instructions.len() < (u32::max_value() - 2) as usize,
            "to many instructions!"
        );
        let id = self.instructions.len() as u32;
        self.instructions.push(instr);
        self.instruction_variables.push(variable);
        SsaVar(id)
    }

    pub fn push_break(&mut self) -> SsaVar {
        let id = self.push(
            VariableId::temporary(),
            Instruction::Jump {
                target: InstrVar::null(),
            },
        );
        self.breaks.push(id);
        id
    }

    pub fn push_continue(&mut self) -> SsaVar {
        let id = self.push(
            VariableId::temporary(),
            Instruction::Jump {
                target: InstrVar::null(),
            },
        );
        self.continues.push(id);
        id
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

    pub fn patch_jump_next(&mut self, instr: SsaVar) {
        self.patch_jump_target(instr, self.next_id().into())
    }

    pub fn push_constant<T: Into<Constant>>(&mut self, variable: VariableId, t: T) -> Expr {
        Expr::Expr(self.load_constant_inner(variable, t.into()))
    }

    pub fn push_binop(
        &mut self,
        variable: VariableId,
        kind: BinOp,
        left: Expr,
        right: Expr,
    ) -> Expr {
        let left = match left{
            Expr::Expr(x) => x,
            Expr::Index
        }
        self.push(variable, Instruction::Binary{
            kind,
        })
    }

    fn expr_into_ssa_var(&mut self, expr: Expr) -> SsaVar{
        match expr{
            Expr::Expr(x) => x,
            Expr::Ident{
                object,
                ident,
            } => {
                let id = self.push_constant(VariableId::temporary(), ident);
                let  res = self.push_temp(
            }
        }
    }

    pub fn take_expr_jump_context(&mut self) -> ExprJumpContext {
        ExprJumpContext {
            true_list: mem::replace(&mut self.true_list, Vec::new()),
            false_list: mem::replace(&mut self.false_list, Vec::new()),
        }
    }

    pub fn take_stmt_jump_context(&mut self) -> StmtJumpContext {
        StmtJumpContext {
            breaks: mem::replace(&mut self.breaks, Vec::new()),
            continues: mem::replace(&mut self.continues, Vec::new()),
        }
    }

    pub fn clear_expr_jump_context(&mut self) {
        self.true_list.clear();
        self.false_list.clear();
    }

    pub fn clear_stmt_jump_context(&mut self) {
        self.breaks.clear();
        self.continues.clear();
    }

    pub fn push_context_jump(&mut self, cond: SsaVar, truthy: bool) -> SsaVar {
        let instr = self.push(
            VariableId::temporary(),
            Instruction::CondJump {
                negative: !truthy,
                condition: cond.into(),
                target: InstrVar::null(),
            },
        );
        if truthy {
            self.true_list.push(instr);
        } else {
            self.false_list.push(instr);
        }
        instr
    }

    pub fn patch_context_jump(&mut self, instr: SsaVar, ctx: &ExprJumpContext, thruthy: bool) {
        let targets = if thruthy {
            &ctx.true_list
        } else {
            &ctx.false_list
        };
        for x in targets.iter().copied() {
            self.patch_jump_target(x, instr.into());
        }
    }

    pub fn patch_break_jump(&mut self, target: SsaVar, ctx: &StmtJumpContext) {
        for b in ctx.breaks.iter() {
            self.patch_jump_target(*b, target.into())
        }
    }

    pub fn patch_continue_jump(&mut self, target: SsaVar, ctx: &StmtJumpContext) {
        for b in ctx.continues.iter() {
            self.patch_jump_target(*b, target.into())
        }
    }

    fn load_constant_inner(&mut self, variable: VariableId, c: Constant) -> SsaVar {
        let const_id = if let Some(x) = self.constant_location.get(&c) {
            *x
        } else {
            let len = self.constants.len();
            self.constants.push(c.clone());
            self.constant_location.insert(c, len);
            len
        };
        assert!(
            self.constants.len() < (u32::max_value() - 2) as usize,
            "to many constants!"
        );
        self.push(
            variable,
            Instruction::LoadConstant {
                constant: ConstantId(const_id as u32),
            },
        )
    }

    pub fn build(self) -> Ssa {
        Ssa {
            instructions: self.instructions,
            constants: self.constants,
        }
    }
}
