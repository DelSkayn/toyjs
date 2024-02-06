use core::panic;

use ast::{ListId, NodeId};
use bc::{Instruction, Reg};

use crate::{Compiler, Error, InstrOffset, Limits, Result};

#[must_use]
pub enum ExprResult {
    Register(Reg),
    InstrDst(InstrOffset),
    Unused,
}

impl ExprResult {
    pub fn to_register(&self, compiler: &mut Compiler) -> Result<Reg> {
        match self {
            ExprResult::InstrDst(instr) => {
                let reg = compiler
                    .registers
                    .alloc_tmp()
                    .ok_or(Error::ExceededLimits(Limits::Registers))?;
                compiler.patch_dst(*instr, reg);
                Ok(reg)
            }
            ExprResult::Register(reg) => Ok(*reg),
            ExprResult::Unused => panic!("used result of expression which should have been unused"),
        }
    }

    pub fn assign_to_reg(&self, compiler: &mut Compiler, reg: Reg) -> Result<()> {
        match self {
            ExprResult::Register(x) => {
                compiler.push(Instruction::Move { dst: reg, src: *x })?;
            }
            ExprResult::InstrDst(instr) => compiler.patch_dst(*instr, reg),
            ExprResult::Unused => {
                panic!("tried to use result which ought to be unused.")
            }
        }
        Ok(())
    }

    pub fn ignore(&self, compiler: &mut Compiler) -> Result<()> {
        match self {
            ExprResult::Register(_) | ExprResult::Unused => Ok(()),
            ExprResult::InstrDst(instr) => {
                let tmp = compiler
                    .registers
                    .alloc_tmp()
                    .ok_or(Error::ExceededLimits(Limits::Registers))?;
                compiler.registers.free_tmp(tmp);
                compiler.patch_dst(*instr, tmp);
                Ok(())
            }
        }
    }
}

impl<'a> Compiler<'a> {
    pub fn compile_exprs(&mut self, mut expr: ListId<ast::Expr>) -> Result<ExprResult> {
        loop {
            let item = self.ast[expr].item;
            if let Some(x) = self.ast[expr].next {
                match self.compile_expr(self.ast[expr].item)? {
                    ExprResult::Register(_) => {}
                    ExprResult::InstrDst(_) => {
                        self.registers
                            .next_free()
                            .ok_or(Error::ExceededLimits(Limits::Registers))?;
                    }
                    ExprResult::Unused => {}
                }
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

                    self.registers.free_if_tmp(left);
                    self.registers.free_if_tmp(right);

                    let dst = Reg::this_reg();

                    let instr = match op {
                        ast::BaseOp::NullCoalessing => to_do!(),
                        ast::BaseOp::TenaryNull => to_do!(),
                        ast::BaseOp::Or => to_do!(),
                        ast::BaseOp::And => to_do!(),
                        ast::BaseOp::BitwiseAnd => {
                            self.push(Instruction::BitAnd { dst, left, right })?
                        }
                        ast::BaseOp::BitwiseOr => {
                            self.push(Instruction::BitOr { dst, left, right })?
                        }
                        ast::BaseOp::BitwiseXor => {
                            self.push(Instruction::BitXor { dst, left, right })?
                        }
                        ast::BaseOp::Add => self.push(Instruction::Add { dst, left, right })?,
                        ast::BaseOp::Sub => self.push(Instruction::Sub { dst, left, right })?,
                        ast::BaseOp::Mul => self.push(Instruction::Mul { dst, left, right })?,
                        ast::BaseOp::Div => self.push(Instruction::Div { dst, left, right })?,
                        ast::BaseOp::Mod => self.push(Instruction::Mod { dst, left, right })?,
                        ast::BaseOp::Exp => self.push(Instruction::Pow { dst, left, right })?,
                        ast::BaseOp::Less => self.push(Instruction::Less { dst, left, right })?,
                        ast::BaseOp::LessEqual => {
                            self.push(Instruction::LessEq { dst, left, right })?
                        }
                        ast::BaseOp::Greater => {
                            self.push(Instruction::Greater { dst, left, right })?
                        }
                        ast::BaseOp::GreaterEqual => {
                            self.push(Instruction::GreaterEq { dst, left, right })?
                        }
                        ast::BaseOp::ShiftLeft => {
                            self.push(Instruction::ShiftL { dst, left, right })?
                        }
                        ast::BaseOp::ShiftRight => {
                            self.push(Instruction::ShiftR { dst, left, right })?
                        }
                        ast::BaseOp::ShiftRightUnsigned => {
                            self.push(Instruction::ShiftRU { dst, left, right })?
                        }
                        ast::BaseOp::InstanceOf => to_do!(),
                        ast::BaseOp::In => to_do!(),
                        ast::BaseOp::Equal => self.push(Instruction::Equal { dst, left, right })?,
                        ast::BaseOp::StrictEqual => {
                            self.push(Instruction::SEqual { dst, left, right })?
                        }
                        ast::BaseOp::NotEqual => {
                            self.push(Instruction::NotEqual { dst, left, right })?
                        }
                        ast::BaseOp::StrictNotEqual => {
                            self.push(Instruction::SNotEqual { dst, left, right })?
                        }
                    };
                    Ok(ExprResult::InstrDst(instr))
                }
                ast::BinaryOp::Assign(_) => to_do!(),
            },
            ast::Expr::Prefix { op, expr } => to_do!(),
            ast::Expr::Postfix { op, expr } => to_do!(),
            ast::Expr::Tenary(_) => to_do!(),
            ast::Expr::Index { index, expr } => to_do!(),
            ast::Expr::Dot { ident, expr } => to_do!(),
            ast::Expr::Call { args, expr } => to_do!(),
            ast::Expr::Prime { expr } => self.compile_prime(expr),
            ast::Expr::Yield { star, expr } => to_do!(),
            ast::Expr::Destructure { pattern, expr } => to_do!(),
            ast::Expr::TaggedTemplate { tag, template } => to_do!(),
        }
    }
}
