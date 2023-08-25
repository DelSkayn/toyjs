use ast::NodeId;
use bc::{Instruction, Reg};

use crate::{Compiler, Result};

impl<'a> Compiler<'a> {
    pub fn compile_prime(&mut self, expr: NodeId<ast::PrimeExpr>) -> Result<i8> {
        match self.ast[expr] {
            ast::PrimeExpr::Number(id) => {
                let number = self.interners.numbers[id];
                let reg = self.register;
                self.register += 1;
                if let Some(imm) = number.cast() {
                    self.instructions
                        .push(Instruction::Loadi8 { dst: Reg(reg), imm })
                } else if let Some(imm) = number.cast() {
                    self.instructions
                        .push(Instruction::Loadi16 { dst: Reg(reg), imm })
                } else if let Some(imm) = number.cast() {
                    self.instructions
                        .push(Instruction::Loadi32 { dst: Reg(reg), imm });
                } else {
                    self.instructions.push(Instruction::Loadf64 {
                        dst: Reg(reg),
                        imm: number.0,
                    });
                }
                Ok(reg)
            }
            ast::PrimeExpr::String(_) => to_do!(),
            ast::PrimeExpr::Template(_) => to_do!(),
            ast::PrimeExpr::Regex(_) => to_do!(),
            ast::PrimeExpr::Ident(_) => to_do!(),
            ast::PrimeExpr::Boolean(_) => to_do!(),
            ast::PrimeExpr::Function(_) => to_do!(),
            ast::PrimeExpr::Class(_) => to_do!(),
            ast::PrimeExpr::Object(_) => to_do!(),
            ast::PrimeExpr::Array(_) => to_do!(),
            ast::PrimeExpr::NewTarget => to_do!(),
            ast::PrimeExpr::Null => to_do!(),
            ast::PrimeExpr::This => to_do!(),
            ast::PrimeExpr::Super => to_do!(),
            ast::PrimeExpr::Covered(_) => to_do!(),
        }
    }
}
