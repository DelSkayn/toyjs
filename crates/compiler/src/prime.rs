use ast::NodeId;
use bc::{Instruction, Reg};

use crate::{Compiler, Result};

impl<'a> Compiler<'a> {
    pub fn compile_prime(
        &mut self,
        placement: Option<Reg>,
        expr: NodeId<ast::PrimeExpr>,
    ) -> Result<Reg> {
        match self.ast[expr] {
            ast::PrimeExpr::Number(id) => {
                let number = self.interners.numbers[id];
                let dst = placement
                    .map(Ok)
                    .unwrap_or_else(|| self.registers.alloc_tmp())?;
                if let Some(imm) = number.cast() {
                    self.instructions.push(Instruction::Loadi8 { dst, imm });
                } else if let Some(imm) = number.cast() {
                    self.instructions.push(Instruction::Loadi16 { dst, imm });
                } else if let Some(imm) = number.cast() {
                    self.instructions.push(Instruction::Loadi32 { dst, imm });
                } else if let Some(imm) = number.cast() {
                    self.instructions.push(Instruction::Loadf32 { dst, imm });
                } else {
                    self.instructions
                        .push(Instruction::Loadf64 { dst, imm: number.0 });
                }
                Ok(dst)
            }
            ast::PrimeExpr::String(_) => to_do!(),
            ast::PrimeExpr::Template(_) => to_do!(),
            ast::PrimeExpr::Regex(_) => to_do!(),
            ast::PrimeExpr::Ident(_) => to_do!(),
            ast::PrimeExpr::Boolean(x) => {
                let dst = placement
                    .map(Ok)
                    .unwrap_or_else(|| self.registers.alloc_tmp())?;
                if x {
                    self.instructions
                        .push(Instruction::LoadPrim { dst, imm: 6 });
                } else {
                    self.instructions
                        .push(Instruction::LoadPrim { dst, imm: 7 });
                }
                Ok(dst)
            }
            ast::PrimeExpr::Function(_) => to_do!(),
            ast::PrimeExpr::Class(_) => to_do!(),
            ast::PrimeExpr::Object(_) => to_do!(),
            ast::PrimeExpr::Array(_) => to_do!(),
            ast::PrimeExpr::NewTarget => Ok(Reg::this_reg()),
            ast::PrimeExpr::Null => {
                let dst = placement
                    .map(Ok)
                    .unwrap_or_else(|| self.registers.alloc_tmp())?;
                self.instructions
                    .push(Instruction::LoadPrim { dst, imm: 2 });
                Ok(dst)
            }
            ast::PrimeExpr::This => Ok(Reg::this_reg()),
            ast::PrimeExpr::Super => to_do!(),
            ast::PrimeExpr::Covered(x) => self.compile_exprs(placement, x),
        }
    }
}
