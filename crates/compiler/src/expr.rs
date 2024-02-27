use core::panic;

use ast::{ListId, NodeId};
use bc::{Instruction, Primitive, Reg};

use crate::{Compiler, InstrOffset, Result};

#[must_use]
pub enum ExprPosition {
    Register(Reg),
    InstrDst(InstrOffset),
    Unused,
}

pub struct ExprResult {
    position: ExprPosition,
    /// TODO: Maybe use list inside array.
    true_jump: Vec<InstrOffset>,
    false_jump: Vec<InstrOffset>,
}

impl From<Reg> for ExprResult {
    fn from(value: Reg) -> Self {
        ExprResult::new(ExprPosition::Register(value))
    }
}

impl From<InstrOffset> for ExprResult {
    fn from(value: InstrOffset) -> Self {
        ExprResult::new(ExprPosition::InstrDst(value))
    }
}

impl From<ExprPosition> for ExprResult {
    fn from(value: ExprPosition) -> Self {
        ExprResult::new(value)
    }
}

impl ExprResult {
    pub fn new(e: ExprPosition) -> Self {
        ExprResult {
            position: e,
            true_jump: Vec::new(),
            false_jump: Vec::new(),
        }
    }

    pub fn to_register(&self, compiler: &mut Compiler) -> Result<Reg> {
        self.patch_jumps(compiler)?;
        match self.position {
            ExprPosition::InstrDst(instr) => {
                let reg = compiler.next_free_register()?;
                compiler.patch_dst(instr, reg);
                Ok(reg)
            }
            ExprPosition::Register(reg) => Ok(reg),
            ExprPosition::Unused => {
                panic!("used result of expression which should have been unused")
            }
        }
    }

    pub fn to_instr(&self, compiler: &mut Compiler) -> Result<InstrOffset> {
        self.patch_jumps(compiler)?;
        match self.position {
            ExprPosition::InstrDst(instr) => Ok(instr),
            ExprPosition::Register(reg) => compiler.emit(Instruction::Move {
                dst: Reg::tmp(),
                src: reg,
            }),
            ExprPosition::Unused => {
                panic!("used result of expression which should have been unused")
            }
        }
    }

    pub fn assign_to_reg(&self, compiler: &mut Compiler, reg: Reg) -> Result<()> {
        self.patch_jumps(compiler)?;
        match self.position {
            ExprPosition::Register(reg) => {
                compiler.emit(Instruction::Move { dst: reg, src: reg })?;
            }
            ExprPosition::InstrDst(instr) => compiler.patch_dst(instr, reg),
            ExprPosition::Unused => {
                panic!("tried to use result which ought to be unused.")
            }
        }
        Ok(())
    }

    pub fn ignore(&self, compiler: &mut Compiler) -> Result<()> {
        self.patch_jumps(compiler)?;
        match self.position {
            ExprPosition::Register(_) | ExprPosition::Unused => Ok(()),
            ExprPosition::InstrDst(instr) => {
                let tmp = compiler.next_free_register()?;
                compiler.patch_dst(instr, tmp);
                Ok(())
            }
        }
    }

    fn patch_jumps(&self, compiler: &mut Compiler) -> Result<()> {
        let target = compiler.next_instruction()?;
        for t in self.true_jump.iter() {
            compiler.patch_jump(*t, target)?
        }
        for t in self.false_jump.iter() {
            compiler.patch_jump(*t, target)?
        }
        Ok(())
    }
}

impl<'a> Compiler<'a> {
    pub fn compile_exprs(&mut self, mut expr: ListId<ast::Expr>) -> Result<ExprResult> {
        loop {
            let item = self.ast[expr].item;
            if let Some(x) = self.ast[expr].next {
                self.compile_expr(self.ast[expr].item)?.ignore(self)?;
                expr = x;
            } else {
                return self.compile_expr(self.ast[expr].item);
            }
        }
    }

    pub fn compile_expr(&mut self, expr: NodeId<ast::Expr>) -> Result<ExprResult> {
        match self.ast[expr] {
            ast::Expr::Binary { op, left, right } => match op {
                ast::BinaryOp::Base(op) => {
                    let left = self.compile_expr(left)?.to_register(self)?;
                    let right = self.compile_expr(right)?.to_register(self)?;

                    self.free_tmp_register(left);
                    self.free_tmp_register(right);

                    let dst = Reg::this_reg();

                    let instr = match op {
                        ast::BaseOp::NullCoalessing => to_do!(),
                        ast::BaseOp::TenaryNull => to_do!(),
                        ast::BaseOp::Or => to_do!(),
                        ast::BaseOp::And => to_do!(),
                        ast::BaseOp::BitwiseAnd => {
                            self.emit(Instruction::BitAnd { dst, left, right })?
                        }
                        ast::BaseOp::BitwiseOr => {
                            self.emit(Instruction::BitOr { dst, left, right })?
                        }
                        ast::BaseOp::BitwiseXor => {
                            self.emit(Instruction::BitXor { dst, left, right })?
                        }
                        ast::BaseOp::Add => self.emit(Instruction::Add { dst, left, right })?,
                        ast::BaseOp::Sub => self.emit(Instruction::Sub { dst, left, right })?,
                        ast::BaseOp::Mul => self.emit(Instruction::Mul { dst, left, right })?,
                        ast::BaseOp::Div => self.emit(Instruction::Div { dst, left, right })?,
                        ast::BaseOp::Mod => self.emit(Instruction::Mod { dst, left, right })?,
                        ast::BaseOp::Exp => self.emit(Instruction::Pow { dst, left, right })?,
                        ast::BaseOp::Less => self.emit(Instruction::Less { dst, left, right })?,
                        ast::BaseOp::LessEqual => {
                            self.emit(Instruction::LessEq { dst, left, right })?
                        }
                        ast::BaseOp::Greater => {
                            self.emit(Instruction::Greater { dst, left, right })?
                        }
                        ast::BaseOp::GreaterEqual => {
                            self.emit(Instruction::GreaterEq { dst, left, right })?
                        }
                        ast::BaseOp::ShiftLeft => {
                            self.emit(Instruction::ShiftL { dst, left, right })?
                        }
                        ast::BaseOp::ShiftRight => {
                            self.emit(Instruction::ShiftR { dst, left, right })?
                        }
                        ast::BaseOp::ShiftRightUnsigned => {
                            self.emit(Instruction::ShiftRU { dst, left, right })?
                        }
                        ast::BaseOp::InstanceOf => to_do!(),
                        ast::BaseOp::In => to_do!(),
                        ast::BaseOp::Equal => self.emit(Instruction::Equal { dst, left, right })?,
                        ast::BaseOp::StrictEqual => {
                            self.emit(Instruction::SEqual { dst, left, right })?
                        }
                        ast::BaseOp::NotEqual => {
                            self.emit(Instruction::NotEqual { dst, left, right })?
                        }
                        ast::BaseOp::StrictNotEqual => {
                            self.emit(Instruction::SNotEqual { dst, left, right })?
                        }
                    };
                    Ok(ExprPosition::InstrDst(instr).into())
                }
                ast::BinaryOp::Assign(_) => to_do!(),
            },
            ast::Expr::Prefix { op, expr } => to_do!(),
            ast::Expr::Postfix { op, expr } => to_do!(),
            ast::Expr::Tenary(_) => to_do!(),
            ast::Expr::Index { index, expr } => to_do!(),
            ast::Expr::Dot { ident, expr } => to_do!(),
            ast::Expr::Call { args, expr } => self.compile_call(expr, args),
            ast::Expr::Prime { expr } => self.compile_prime(expr),
            ast::Expr::Yield { star, expr } => to_do!(),
            ast::Expr::Destructure { pattern, expr } => to_do!(),
            ast::Expr::TaggedTemplate { tag, template } => to_do!(),
        }
    }

    pub fn compile_call(
        &mut self,
        func: NodeId<ast::Expr>,
        args: Option<NodeId<ast::NodeList<ast::Argument>>>,
    ) -> Result<ExprResult> {
        let expr = self.compile_expr(func)?.to_instr(self)?;
        self.push_arg(expr, 0);
        // TODO: this value
        let instr = self.emit(Instruction::LoadPrim {
            dst: Reg::tmp(),
            imm: Primitive::undefined(),
        })?;
        self.push_arg(instr, 1);

        let mut cur = args;
        let mut offset = 2;
        while let Some(n) = cur {
            let arg = &self.ast[n].data;
            if arg.is_spread {
                to_do!()
            }

            let arg = self.compile_expr(arg.expr)?.to_instr(self)?;
            self.push_arg(arg, offset);
            offset += 1;

            cur = self.ast[n].next;
        }

        let instr = self.emit(Instruction::Call {
            dst: Reg::tmp(),
            argc: offset,
        })?;

        Ok(ExprResult::new(ExprPosition::InstrDst(instr)))
    }
}
