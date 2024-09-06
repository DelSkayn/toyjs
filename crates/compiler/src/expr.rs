use core::panic;

use ast::{AssignOp, NodeId, NodeListId};
use bc::{Instruction, LongOffset, OpCode, Primitive, Reg};

use crate::{Compiler, InstrOffset, Result};

#[derive(Debug)]
#[must_use]
pub enum ExprPosition {
    Register(Reg),
    InstrDst(InstrOffset),
    Unused,
}

#[derive(Debug)]
#[must_use]
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

    pub fn to_cond_register(&self, compiler: &mut Compiler) -> Result<Reg> {
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

    pub fn into_register(mut self, compiler: &mut Compiler) -> Result<Reg> {
        self.patch_jumps(compiler)?;
        match self.position {
            ExprPosition::InstrDst(instr) => {
                let reg = compiler.alloc_tmp_register()?;
                compiler.patch_dst(instr, reg);
                Ok(reg)
            }
            ExprPosition::Register(reg) => Ok(reg),
            ExprPosition::Unused => {
                panic!("used result of expression which should have been unused")
            }
        }
    }

    pub fn into_instr(mut self, compiler: &mut Compiler) -> Result<InstrOffset> {
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

    pub fn assign_to_reg(mut self, compiler: &mut Compiler, reg: Reg) -> Result<()> {
        self.patch_jumps(compiler)?;
        match self.position {
            ExprPosition::Register(old_reg) => {
                compiler.emit_move(reg, old_reg)?;
            }
            ExprPosition::InstrDst(instr) => compiler.patch_dst(instr, reg),
            ExprPosition::Unused => {
                panic!("tried to use result which ought to be unused.")
            }
        }
        Ok(())
    }

    pub fn assign_to_cond_reg(&self, compiler: &mut Compiler, reg: Reg) -> Result<()> {
        match self.position {
            ExprPosition::Register(old_reg) => {
                compiler.emit_move(reg, old_reg)?;
            }
            ExprPosition::InstrDst(instr) => compiler.patch_dst(instr, reg),
            ExprPosition::Unused => {
                panic!("tried to use result which ought to be unused.")
            }
        }
        Ok(())
    }

    pub fn ignore(mut self, compiler: &mut Compiler) -> Result<()> {
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

    pub fn patch_jumps(&mut self, compiler: &mut Compiler) -> Result<()> {
        let next = compiler.next_instruction()?;
        self.patch_true_jumps_to(compiler, next)?;
        self.patch_false_jumps_to(compiler, next)
    }

    pub fn patch_true_jumps_to(&mut self, compiler: &mut Compiler, tgt: InstrOffset) -> Result<()> {
        for t in self.true_jump.drain(..) {
            compiler.patch_jump(t, tgt)?
        }
        Ok(())
    }

    pub fn patch_false_jumps_to(
        &mut self,
        compiler: &mut Compiler,
        tgt: InstrOffset,
    ) -> Result<()> {
        for t in self.false_jump.drain(..) {
            compiler.patch_jump(t, tgt)?
        }
        Ok(())
    }

    pub fn merge_jumps_from(mut self, other: Self) -> Self {
        self.true_jump.extend(other.true_jump);
        self.false_jump.extend(other.false_jump);
        self
    }

    pub fn with_true_jump(mut self, jmp: InstrOffset) -> Self {
        self.true_jump.push(jmp);
        self
    }
    pub fn with_false_jump(mut self, jmp: InstrOffset) -> Self {
        self.false_jump.push(jmp);
        self
    }
}

impl<'a> Compiler<'a> {
    pub fn compile_exprs(&mut self, mut expr: NodeListId<ast::Expr>) -> Result<ExprResult> {
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
                    let opcode = match op {
                        ast::BaseOp::NullCoalessing => to_do!(),
                        ast::BaseOp::TenaryNull => to_do!(),
                        ast::BaseOp::Or => {
                            let mut left = self.compile_expr(left)?;
                            let left_reg = left.to_cond_register(self)?;
                            let left_reg = if !self.regs.is_tmp(left_reg) {
                                let reg = self.alloc_tmp_register()?;
                                self.emit_move(reg, left_reg)?;
                                reg
                            } else {
                                left_reg
                            };
                            let jump = self.emit(Instruction::LongJumpTrue {
                                cond: left_reg,
                                dst: LongOffset(0),
                            })?;

                            let next = self.next_instruction()?;
                            left.patch_false_jumps_to(self, next)?;

                            let right = self.compile_expr(right)?;
                            right.assign_to_cond_reg(self, left_reg)?;

                            return Ok(ExprResult::from(left_reg)
                                .merge_jumps_from(right)
                                .merge_jumps_from(left)
                                .with_true_jump(jump));
                        }
                        ast::BaseOp::And => {
                            let mut left = self.compile_expr(left)?;
                            let left_reg = left.to_cond_register(self)?;
                            let left_reg = if !self.regs.is_tmp(left_reg) {
                                let reg = self.alloc_tmp_register()?;
                                self.emit_move(reg, left_reg)?;
                                reg
                            } else {
                                left_reg
                            };
                            let jump = self.emit(Instruction::LongJumpFalse {
                                cond: left_reg,
                                dst: LongOffset(0),
                            })?;

                            let next = self.next_instruction()?;
                            left.patch_true_jumps_to(self, next)?;

                            let right = self.compile_expr(right)?;
                            right.assign_to_cond_reg(self, left_reg)?;

                            return Ok(ExprResult::from(left_reg)
                                .merge_jumps_from(right)
                                .merge_jumps_from(left)
                                .with_false_jump(jump));
                        }
                        ast::BaseOp::BitwiseAnd => OpCode::BitAnd,
                        ast::BaseOp::BitwiseOr => OpCode::BitOr,
                        ast::BaseOp::BitwiseXor => OpCode::BitXor,
                        ast::BaseOp::Add => OpCode::Add,
                        ast::BaseOp::Sub => OpCode::Sub,
                        ast::BaseOp::Mul => OpCode::Mul,
                        ast::BaseOp::Div => OpCode::Div,
                        ast::BaseOp::Mod => OpCode::Mod,
                        ast::BaseOp::Exp => OpCode::Pow,
                        ast::BaseOp::Less => OpCode::Less,
                        ast::BaseOp::LessEqual => OpCode::LessEq,
                        ast::BaseOp::Greater => OpCode::Greater,
                        ast::BaseOp::GreaterEqual => OpCode::GreaterEq,
                        ast::BaseOp::ShiftLeft => OpCode::ShiftL,
                        ast::BaseOp::ShiftRight => OpCode::ShiftR,
                        ast::BaseOp::ShiftRightUnsigned => OpCode::ShiftRU,
                        ast::BaseOp::InstanceOf => to_do!(),
                        ast::BaseOp::In => to_do!(),
                        ast::BaseOp::Equal => OpCode::Equal,
                        ast::BaseOp::StrictEqual => OpCode::SEqual,
                        ast::BaseOp::NotEqual => OpCode::NotEqual,
                        ast::BaseOp::StrictNotEqual => OpCode::SNotEqual,
                    };

                    let left = self.compile_expr(left)?.into_register(self)?;
                    let right = self.compile_expr(right)?.into_register(self)?;

                    self.free_tmp_register(left);
                    self.free_tmp_register(right);

                    let dst = Reg::this_reg();
                    let instr = self.next_instruction()?;
                    self.push_instr_byte(opcode as u8)?;
                    self.push_instr_byte(dst)?;
                    self.push_instr_byte(left)?;
                    self.push_instr_byte(right)?;
                    Ok(ExprPosition::InstrDst(instr).into())
                }
                ast::BinaryOp::Assign(op) => self.compile_assign_expr(op, left, right),
            },
            ast::Expr::Prefix { op, expr } => match op {
                ast::PrefixOp::AddOne => to_do!(),
                ast::PrefixOp::SubOne => to_do!(),
                ast::PrefixOp::Plus => {
                    let expr = self.compile_expr(expr)?.into_register(self)?;
                    self.free_tmp_register(expr);
                    let dst = self.alloc_tmp_register()?;
                    self.emit(Instruction::ToNum { dst, src: expr })?;
                    Ok(dst.into())
                }
                ast::PrefixOp::Minus => {
                    let expr = self.compile_expr(expr)?.into_register(self)?;
                    self.free_tmp_register(expr);
                    let dst = self.alloc_tmp_register()?;
                    self.emit(Instruction::Neg { dst, src: expr })?;
                    Ok(dst.into())
                }
                ast::PrefixOp::Not => {
                    let expr = self.compile_expr(expr)?.into_register(self)?;
                    self.free_tmp_register(expr);
                    let dst = self.alloc_tmp_register()?;
                    self.emit(Instruction::Not { dst, src: expr })?;
                    Ok(dst.into())
                }
                ast::PrefixOp::BitwiseNot => {
                    let expr = self.compile_expr(expr)?.into_register(self)?;
                    self.free_tmp_register(expr);
                    let dst = self.alloc_tmp_register()?;
                    self.emit(Instruction::BitNot { dst, src: expr })?;
                    Ok(dst.into())
                }
                ast::PrefixOp::New => to_do!(),
                ast::PrefixOp::Delete => to_do!(),
                ast::PrefixOp::Void => to_do!(),
                ast::PrefixOp::TypeOf => to_do!(),
                ast::PrefixOp::Await => to_do!(),
            },
            ast::Expr::Postfix { op, expr } => match op {
                ast::PostfixOp::AddOne => to_do!(),
                ast::PostfixOp::SubOne => to_do!(),
            },
            ast::Expr::Ternary { ternary } => {
                let tenary = self.ast[ternary];
                let mut cond = self.compile_expr(tenary.cond)?;
                let cond_reg = cond.to_cond_register(self)?;
                let cond_jump = self.emit(Instruction::LongJumpFalse {
                    cond: cond_reg,
                    dst: LongOffset(0),
                })?;
                let next = self.next_instruction()?;
                cond.patch_true_jumps_to(self, next)?;
                let then = self.compile_expr(tenary.then)?.into_register(self)?;
                let then = if self.regs.is_tmp(then) {
                    then
                } else {
                    let new_then = self.alloc_tmp_register()?;
                    self.emit_move(new_then, then)?;
                    new_then
                };
                let then_jump = self.emit(Instruction::LongJump { dst: LongOffset(0) })?;
                let next = self.next_instruction()?;
                cond.patch_false_jumps_to(self, next)?;
                self.patch_jump(cond_jump, next)?;
                self.compile_expr(tenary.r#else)?
                    .assign_to_reg(self, then)?;
                let next = self.next_instruction()?;
                self.patch_jump(then_jump, next)?;
                Ok(then.into())
            }
            ast::Expr::Index { index, expr } => {
                let expr = self.compile_expr(expr)?.into_register(self)?;
                let index = self.compile_expr(index)?.into_register(self)?;
                self.free_tmp_register(expr);
                self.free_tmp_register(index);
                let dst = self.alloc_tmp_register()?;
                self.emit(Instruction::IndexLoad {
                    dst,
                    obj: expr,
                    key: index,
                })?;
                Ok(dst.into())
            }
            ast::Expr::Dot { ident, expr } => {
                let expr = self.compile_expr(expr)?.into_register(self)?;
                let str = self.compile_string(ident)?;
                let key = self.alloc_tmp_register()?;
                self.free_tmp_register(expr);
                self.patch_dst(str, key);
                self.emit(Instruction::IndexLoad {
                    dst: key,
                    obj: expr,
                    key,
                })?;
                Ok(key.into())
            }
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
        let expr = self.compile_expr(func)?.into_instr(self)?;
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
            let arg = self.ast[n].item;
            if self.ast[arg].is_spread {
                to_do!()
            }

            let arg = self.compile_expr(self.ast[arg].expr)?.into_instr(self)?;
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

    pub fn compile_assign_expr(
        &mut self,
        op: AssignOp,
        mut left: NodeId<ast::Expr>,
        right: NodeId<ast::Expr>,
    ) -> Result<ExprResult> {
        let right = self.compile_expr(right)?.into_register(self)?;
        let (obj, key) = loop {
            match self.ast[left] {
                ast::Expr::Binary { .. }
                | ast::Expr::Prefix { .. }
                | ast::Expr::Postfix { .. }
                | ast::Expr::Ternary { .. }
                | ast::Expr::Yield { .. }
                | ast::Expr::Destructure { .. }
                | ast::Expr::TaggedTemplate { .. }
                | ast::Expr::Call { .. } => unreachable!(),
                ast::Expr::Index { index, expr } => {
                    let obj = self.compile_expr(expr)?.into_register(self)?;
                    let key = self.compile_expr(index)?.into_register(self)?;
                    break (obj, key);
                }
                ast::Expr::Dot { ident, expr } => {
                    let obj = self.compile_expr(expr)?.into_register(self)?;
                    let key = ExprResult::from(self.compile_string(ident)?).into_register(self)?;
                    break (obj, key);
                }
                ast::Expr::Prime { expr } => match self.ast[expr] {
                    ast::PrimeExpr::Number { .. }
                    | ast::PrimeExpr::String { .. }
                    | ast::PrimeExpr::Template { .. }
                    | ast::PrimeExpr::Regex { .. }
                    | ast::PrimeExpr::Boolean { .. }
                    | ast::PrimeExpr::Function { .. }
                    | ast::PrimeExpr::Class { .. }
                    | ast::PrimeExpr::Object { .. }
                    | ast::PrimeExpr::Array { .. }
                    | ast::PrimeExpr::NewTarget
                    | ast::PrimeExpr::Null
                    | ast::PrimeExpr::This
                    | ast::PrimeExpr::Super => unreachable!(),
                    ast::PrimeExpr::Ident { symbol } => {
                        let sym = self.variables.symbol_of_ast(symbol);
                        self.store_symbol(sym, right.into())?;
                        return Ok(right.into());
                    }
                    ast::PrimeExpr::Covered { expr } => {
                        left = self.ast[expr].item;
                    }
                },
            };
        };

        let idx = match op {
            AssignOp::Assign => {
                self.emit(Instruction::IndexStore {
                    obj,
                    key,
                    src: right,
                })?;
                return Ok(right.into());
            }
            AssignOp::Add => OpCode::Add,
            AssignOp::Sub => OpCode::Sub,
            AssignOp::Mul => OpCode::Mul,
            AssignOp::Div => OpCode::Div,
            AssignOp::Mod => OpCode::Mod,
            AssignOp::Exp => OpCode::Pow,
            AssignOp::ShiftLeft => OpCode::ShiftL,
            AssignOp::ShiftRight => OpCode::ShiftR,
            AssignOp::ShiftRightUnsigned => OpCode::ShiftRU,
            AssignOp::BitwiseAnd => OpCode::BitAnd,
            AssignOp::BitwiseOr => OpCode::BitOr,
            AssignOp::BitwiseXor => OpCode::BitXor,
        };

        let dst = self.alloc_tmp_register()?;
        self.emit(Instruction::IndexLoad { dst, obj, key })?;
        self.free_tmp_register(right);
        self.push_instr_byte(idx as u8)?;
        self.push_instr_byte(dst)?;
        self.push_instr_byte(dst)?;
        self.push_instr_byte(right)?;
        self.free_tmp_register(obj);
        self.free_tmp_register(key);
        self.emit(Instruction::IndexStore { obj, key, src: dst })?;
        Ok(dst.into())
    }
}
