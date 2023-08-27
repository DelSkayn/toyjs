use ast::{ListId, NodeId};
use bc::{Instruction, Reg};

use crate::{Compiler, Result};

impl<'a> Compiler<'a> {
    pub fn compile_exprs(
        &mut self,
        valid: Option<NodeId<ast::Expr>>,
        mut expr: ListId<ast::Expr>,
    ) -> Result<Reg> {
        loop {
            let item = self.ast[expr].item;
            let res = self.compile_expr(valid.unwrap_or(item), self.ast[expr].item)?;
            if let Some(e) = self.ast[expr].next {
                self.registers.free(res);
                expr = e;
            } else {
                return Ok(res);
            }
        }
    }

    pub fn compile_expr(
        &mut self,
        valid: NodeId<ast::Expr>,
        expr: NodeId<ast::Expr>,
    ) -> Result<Reg> {
        match self.ast[expr] {
            ast::Expr::Binary { op, left, right } => match op {
                ast::BinaryOp::Base(op) => {
                    let left = self.compile_expr(right, left)?;
                    let right = self.compile_expr(right, right)?;
                    let dst = self.registers.alloc_tmp(expr, valid)?;
                    match op {
                        ast::BaseOp::NullCoalessing => to_do!(),
                        ast::BaseOp::TenaryNull => to_do!(),
                        ast::BaseOp::Or => to_do!(),
                        ast::BaseOp::And => to_do!(),
                        ast::BaseOp::BitwiseAnd => {
                            self.instructions
                                .push(Instruction::BitAnd { dst, left, right });
                        }
                        ast::BaseOp::BitwiseOr => {
                            self.instructions
                                .push(Instruction::BitOr { dst, left, right });
                        }
                        ast::BaseOp::BitwiseXor => {
                            self.instructions
                                .push(Instruction::BitXor { dst, left, right });
                        }
                        ast::BaseOp::Add => {
                            self.instructions
                                .push(Instruction::Add { dst, left, right });
                        }
                        ast::BaseOp::Sub => {
                            self.instructions
                                .push(Instruction::Sub { dst, left, right });
                        }
                        ast::BaseOp::Mul => {
                            self.instructions
                                .push(Instruction::Mul { dst, left, right });
                        }
                        ast::BaseOp::Div => {
                            self.instructions
                                .push(Instruction::Div { dst, left, right });
                        }
                        ast::BaseOp::Mod => {
                            self.instructions
                                .push(Instruction::Mod { dst, left, right });
                        }
                        ast::BaseOp::Exp => {
                            self.instructions
                                .push(Instruction::Pow { dst, left, right });
                        }
                        ast::BaseOp::Less => {
                            self.instructions
                                .push(Instruction::Less { dst, left, right });
                        }
                        ast::BaseOp::LessEqual => {
                            self.instructions
                                .push(Instruction::LessEq { dst, left, right });
                        }
                        ast::BaseOp::Greater => {
                            self.instructions
                                .push(Instruction::Greater { dst, left, right });
                        }
                        ast::BaseOp::GreaterEqual => {
                            self.instructions
                                .push(Instruction::GreaterEq { dst, left, right });
                        }
                        ast::BaseOp::ShiftLeft => {
                            self.instructions
                                .push(Instruction::ShiftL { dst, left, right });
                        }
                        ast::BaseOp::ShiftRight => {
                            self.instructions
                                .push(Instruction::ShiftR { dst, left, right });
                        }
                        ast::BaseOp::ShiftRightUnsigned => {
                            self.instructions
                                .push(Instruction::ShiftRU { dst, left, right });
                        }
                        ast::BaseOp::InstanceOf => to_do!(),
                        ast::BaseOp::In => to_do!(),
                        ast::BaseOp::Equal => {
                            self.instructions
                                .push(Instruction::Equal { dst, left, right });
                        }
                        ast::BaseOp::StrictEqual => {
                            self.instructions
                                .push(Instruction::SEqual { dst, left, right });
                        }
                        ast::BaseOp::NotEqual => {
                            self.instructions
                                .push(Instruction::NotEqual { dst, left, right });
                        }
                        ast::BaseOp::StrictNotEqual => {
                            self.instructions
                                .push(Instruction::SNotEqual { dst, left, right });
                        }
                    }
                    Ok(dst)
                }
                ast::BinaryOp::Assign(_) => to_do!(),
            },
            ast::Expr::Prefix { op, expr } => to_do!(),
            ast::Expr::Postfix { op, expr } => to_do!(),
            ast::Expr::Tenary(_) => to_do!(),
            ast::Expr::Index { index, expr } => to_do!(),
            ast::Expr::Dot { ident, expr } => to_do!(),
            ast::Expr::Call { args, expr } => to_do!(),
            ast::Expr::Prime { expr } => self.compile_prime(valid, expr),
            ast::Expr::Yield { star, expr } => to_do!(),
            ast::Expr::Destructure { pattern, expr } => to_do!(),
            ast::Expr::TaggedTemplate { tag, template } => to_do!(),
        }
    }
}
