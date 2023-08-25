use ast::NodeId;
use bc::{Instruction, Reg};

use crate::{Compiler, Result};

impl<'a> Compiler<'a> {
    pub fn compile_expr(&mut self, expr: NodeId<ast::Expr>) -> Result<i8> {
        match self.ast[expr] {
            ast::Expr::Binary { op, left, right } => match op {
                ast::BinaryOp::Base(op) => {
                    let left = self.compile_expr(left)?;
                    let right = self.compile_expr(right)?;
                    let reg = self.register;
                    self.register += 1;
                    match op {
                        ast::BaseOp::NullCoalessing => to_do!(),
                        ast::BaseOp::TenaryNull => to_do!(),
                        ast::BaseOp::Or => to_do!(),
                        ast::BaseOp::And => to_do!(),
                        ast::BaseOp::BitwiseAnd => {
                            self.instructions.push(Instruction::BitAnd {
                                dst: Reg(reg),
                                left: Reg(left),
                                right: Reg(right),
                            });
                        }
                        ast::BaseOp::BitwiseOr => {
                            self.instructions.push(Instruction::BitOr {
                                dst: Reg(reg),
                                left: Reg(left),
                                right: Reg(right),
                            });
                        }
                        ast::BaseOp::BitwiseXor => {
                            self.instructions.push(Instruction::BitXor {
                                dst: Reg(reg),
                                left: Reg(left),
                                right: Reg(right),
                            });
                        }
                        ast::BaseOp::Add => {
                            self.instructions.push(Instruction::Add {
                                dst: Reg(reg),
                                left: Reg(left),
                                right: Reg(right),
                            });
                        }
                        ast::BaseOp::Sub => {
                            self.instructions.push(Instruction::Sub {
                                dst: Reg(reg),
                                left: Reg(left),
                                right: Reg(right),
                            });
                        }
                        ast::BaseOp::Mul => {
                            self.instructions.push(Instruction::Mul {
                                dst: Reg(reg),
                                left: Reg(left),
                                right: Reg(right),
                            });
                        }
                        ast::BaseOp::Div => {
                            self.instructions.push(Instruction::Div {
                                dst: Reg(reg),
                                left: Reg(left),
                                right: Reg(right),
                            });
                        }
                        ast::BaseOp::Mod => {
                            self.instructions.push(Instruction::Mod {
                                dst: Reg(reg),
                                left: Reg(left),
                                right: Reg(right),
                            });
                        }
                        ast::BaseOp::Exp => {
                            self.instructions.push(Instruction::Pow {
                                dst: Reg(reg),
                                left: Reg(left),
                                right: Reg(right),
                            });
                        }
                        ast::BaseOp::Less => {
                            self.instructions.push(Instruction::Less {
                                dst: Reg(reg),
                                left: Reg(left),
                                right: Reg(right),
                            });
                        }
                        ast::BaseOp::LessEqual => {
                            self.instructions.push(Instruction::LessEq {
                                dst: Reg(reg),
                                left: Reg(left),
                                right: Reg(right),
                            });
                        }
                        ast::BaseOp::Greater => {
                            self.instructions.push(Instruction::Greater {
                                dst: Reg(reg),
                                left: Reg(left),
                                right: Reg(right),
                            });
                        }
                        ast::BaseOp::GreaterEqual => {
                            self.instructions.push(Instruction::GreaterEq {
                                dst: Reg(reg),
                                left: Reg(left),
                                right: Reg(right),
                            });
                        }
                        ast::BaseOp::ShiftLeft => {
                            self.instructions.push(Instruction::ShiftL {
                                dst: Reg(reg),
                                left: Reg(left),
                                right: Reg(right),
                            });
                        }
                        ast::BaseOp::ShiftRight => {
                            self.instructions.push(Instruction::ShiftR {
                                dst: Reg(reg),
                                left: Reg(left),
                                right: Reg(right),
                            });
                        }
                        ast::BaseOp::ShiftRightUnsigned => {
                            self.instructions.push(Instruction::ShiftRU {
                                dst: Reg(reg),
                                left: Reg(left),
                                right: Reg(right),
                            });
                        }
                        ast::BaseOp::InstanceOf => to_do!(),
                        ast::BaseOp::In => to_do!(),
                        ast::BaseOp::Equal => {
                            self.instructions.push(Instruction::Equal {
                                dst: Reg(reg),
                                left: Reg(left),
                                right: Reg(right),
                            });
                        }
                        ast::BaseOp::StrictEqual => {
                            self.instructions.push(Instruction::SEqual {
                                dst: Reg(reg),
                                left: Reg(left),
                                right: Reg(right),
                            });
                        }
                        ast::BaseOp::NotEqual => {
                            self.instructions.push(Instruction::NotEqual {
                                dst: Reg(reg),
                                left: Reg(left),
                                right: Reg(right),
                            });
                        }
                        ast::BaseOp::StrictNotEqual => {
                            self.instructions.push(Instruction::SNotEqual {
                                dst: Reg(reg),
                                left: Reg(left),
                                right: Reg(right),
                            });
                        }
                    }
                    Ok(reg)
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
            ast::Expr::TaggedTemplate { tag, template } => to_do!(),
        }
    }
}
