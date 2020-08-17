use crate::{
    interner::StringId,
    ssa::{
        BinOp, Constant, ConstantId, Instruction, OptionSsaId, Ssa, SsaFunction, SsaId, UnaryOp,
    },
    util::Integer,
};
use std::mem;
mod variable;
use fxhash::FxHashMap;
pub use variable::{BindingType, VariableId, VariableTable, Variables};

#[derive(Clone, Copy)]
pub enum Expr {
    Expr(SsaId),
    Variable(VariableId),
    /// an expression in the form of obj[value]
    Ident {
        object: SsaId,
        name: StringId,
    },
    /// an expression in the form of obj[value]
    Index {
        object: SsaId,
        value: SsaId,
    },
}

impl Expr {
    pub fn is_assignable(&self) -> bool {
        match *self {
            Expr::Expr(_) => false,
            Expr::Variable(_) => true,
            Expr::Ident { object: _, name: _ } => true,
            Expr::Index {
                object: _,
                value: _,
            } => true,
        }
    }
}

impl From<SsaId> for Expr {
    fn from(v: SsaId) -> Self {
        Expr::Expr(v)
    }
}

pub struct SsaFactory {
    variables: VariableTable,
    functions: Vec<SsaFunction>,
    constant_id: FxHashMap<Constant, ConstantId>,
    constants: Vec<Constant>,
}

impl SsaFactory {
    pub fn new() -> SsaFactory {
        SsaFactory {
            variables: VariableTable::new(),
            functions: vec![SsaFunction::new(None)],
            constant_id: FxHashMap::default(),
            constants: Vec::new(),
        }
    }

    pub fn create_builder(&mut self) -> SsaBuilder {
        SsaBuilder {
            cur_function: self.functions.len() - 1,
            variables: &mut self.variables,
            instruction_buffer: &mut self.functions,
            expr_ctx: ExprContext::default(),
            stmt_ctx: StmtContext::default(),
        }
    }

    pub fn finish(self) -> Ssa {
        Ssa {
            variables: self.variables.into_variables(),
            functions: self.functions,
            constants: self.constants,
        }
    }
}

#[derive(Default)]
pub struct ExprContext {
    // TODO: these should prob be shortvec's
    true_jumps: Vec<SsaId>,
    false_jumps: Vec<SsaId>,
}

#[derive(Default)]
pub struct StmtContext {
    breaks: Vec<SsaId>,
    continues: Vec<SsaId>,
}

pub struct SsaBuilder<'a> {
    variables: &'a mut VariableTable,
    instruction_buffer: &'a mut Vec<SsaFunction>,
    cur_function: usize,
    expr_ctx: ExprContext,
    stmt_ctx: StmtContext,
}

impl<'a> SsaBuilder<'a> {
    /// Returns the next Ssa Id
    pub fn next(&self) -> SsaId {
        SsaId::from(self.instruction_buffer.len())
    }

    pub fn constant(&mut self, _constant: Constant) -> Expr {
        todo!()
    }

    pub fn enable_strict(&mut self) {
        self.instruction_buffer[self.cur_function].strict = true;
    }

    /// Declare a variable.
    /// Returns None if the variable could not be redeclared
    pub fn reference(&mut self, name: StringId) -> Expr {
        Expr::Variable(self.variables.reference(name))
    }

    /// Declare a variable.
    /// Returns None if the variable could not be redeclared
    pub fn declare(&mut self, name: StringId, ty: BindingType) -> Option<Expr> {
        self.variables
            .declare(name, ty)
            .into_option()
            .map(Expr::Variable)
    }

    pub fn alias(&mut self, left: Expr, right: Expr) -> Expr {
        let left = self.evaluate(left);
        let right = self.evaluate(right);
        Expr::Expr(self.push(Instruction::Alias { left, right }))
    }

    pub fn bin_op(&mut self, kind: BinOp, left: Expr, right: Expr) -> Expr {
        let left = self.evaluate(left);
        let right = self.evaluate(right);
        Expr::Expr(self.push(Instruction::Binary { kind, left, right }))
    }

    pub fn unary_op(&mut self, kind: UnaryOp, operand: Expr) -> Expr {
        let operand = self.evaluate(operand);
        Expr::Expr(self.push(Instruction::Unary { kind, operand }))
    }

    pub fn push_return(&mut self, value: OptionSsaId) {
        self.push(Instruction::Return { value });
    }

    pub fn assign(&mut self, target: Expr, rhs: Expr) {
        let rhs = self.evaluate(rhs);
        match target {
            Expr::Expr(_) => panic!("tried to assign to non assignable expression, check before calling builder functions"),
            Expr::Variable(x) => {
                self.push(Instruction::SetVariable{ variable: x, value: rhs});
            }
            Expr::Ident { object, name } => {
                let constant = self.constant(Constant::String(name));
                let key = self.evaluate(constant);
                self.push(Instruction::ObjectSet {
                    object,
                    key,
                    value: rhs,
                });
            }
            Expr::Index { object, value } => {
                let key = value;
                self.push(Instruction::ObjectSet {
                    object,
                    key,
                    value: rhs,
                });
            }
        }
    }

    pub fn jump(&mut self, target: OptionSsaId) -> SsaId {
        self.push(Instruction::Jump { target })
    }

    pub fn jump_cond(&mut self, target: OptionSsaId, condition: Expr, jump_on_true: bool) -> SsaId {
        let condition = self.evaluate(condition);
        self.push(Instruction::CondJump {
            target,
            condition,
            thruthy: jump_on_true,
        })
    }

    pub fn jump_context(&mut self, target: OptionSsaId, thruthy: bool) -> SsaId {
        let res = self.push(Instruction::Jump { target });
        if thruthy {
            self.expr_ctx.true_jumps.push(res);
        } else {
            self.expr_ctx.false_jumps.push(res);
        }
        res
    }

    pub fn jump_cond_context(
        &mut self,
        target: OptionSsaId,
        condition: Expr,
        jump_on_true: bool,
        thruthy: bool,
    ) -> SsaId {
        let condition = self.evaluate(condition);
        let res = self.push(Instruction::CondJump {
            target,
            condition,
            thruthy: jump_on_true,
        });
        if thruthy {
            self.expr_ctx.true_jumps.push(res);
        } else {
            self.expr_ctx.false_jumps.push(res);
        }
        res
    }

    pub fn jump_break(&mut self) {
        let id = self.next();
        self.jump(OptionSsaId::none());
        self.stmt_ctx.breaks.push(id);
    }

    pub fn jump_continue(&mut self) {
        let id = self.next();
        self.jump(OptionSsaId::none());
        self.stmt_ctx.continues.push(id);
    }

    pub fn patch_jump(&mut self, jump: SsaId, target: SsaId) {
        let new_target = target;
        match self.function().instructions[jump.into_usize()] {
            Instruction::CondJump {
                thruthy: _,
                condition: _,
                ref mut target,
            } => *target = OptionSsaId::some(new_target),
            Instruction::Jump { ref mut target } => *target = OptionSsaId::some(new_target),
            _ => panic!("ssa instruction not a jump"),
        }
    }

    pub fn patch_expr_jump(&mut self, expr_ctx: &ExprContext, target: SsaId, thruthy: bool) {
        let jumps = if thruthy {
            &expr_ctx.true_jumps
        } else {
            &expr_ctx.false_jumps
        };
        for j in jumps.iter().copied() {
            self.patch_jump(j, target)
        }
    }

    pub fn patch_break_jump(&mut self, stmt_ctx: &StmtContext, target: SsaId) {
        let jumps = &stmt_ctx.breaks;
        for j in jumps.iter().copied() {
            self.patch_jump(j, target)
        }
    }

    pub fn patch_continue_jump(&mut self, stmt_ctx: &StmtContext, target: SsaId) {
        let jumps = &stmt_ctx.continues;
        for j in jumps.iter().copied() {
            self.patch_jump(j, target)
        }
    }

    pub fn take_expr_context(&mut self) -> ExprContext {
        mem::replace(&mut self.expr_ctx, ExprContext::default())
    }

    pub fn take_stmt_context(&mut self) -> StmtContext {
        mem::replace(&mut self.stmt_ctx, StmtContext::default())
    }

    pub fn evaluate(&mut self, expr: Expr) -> SsaId {
        match expr {
            Expr::Expr(x) => x,
            Expr::Ident { object, name } => {
                let constant = self.constant(Constant::String(name));
                let key = self.evaluate(constant);
                self.push(Instruction::ObjectGet { object, key })
            }
            Expr::Index { object, value } => {
                self.push(Instruction::ObjectGet { object, key: value })
            }
            Expr::Variable(x) => self.push(Instruction::GetVariable { variable: x }),
        }
    }

    pub fn push_scope(&mut self) {
        self.variables.push_scope();
    }

    pub fn pop_scope(&mut self) {
        self.variables.pop_scope();
    }

    fn push(&mut self, instr: Instruction) -> SsaId {
        let f = self.function();
        let idx = f.instructions.len();
        f.instructions.push(instr);
        SsaId::from(idx)
    }

    fn function(&mut self) -> &mut SsaFunction {
        &mut self.instruction_buffer[self.cur_function]
    }
}
