use ast::NodeId;
use bc::{Instruction, Reg};

use crate::{expr::ExprResult, Compiler, Result};

impl<'a> Compiler<'a> {
    pub fn compile_prime(&mut self, expr: NodeId<ast::PrimeExpr>) -> Result<ExprResult> {
        let dst = Reg::this_reg();
        match self.ast[expr] {
            ast::PrimeExpr::Number(id) => {
                let number = self.interners.numbers[id];
                let instr = if let Some(imm) = number.cast() {
                    self.push(Instruction::Loadi8 { dst, imm })?
                } else if let Some(imm) = number.cast() {
                    self.push(Instruction::Loadi16 { dst, imm })?
                } else if let Some(imm) = number.cast() {
                    self.push(Instruction::Loadi32 { dst, imm })?
                } else if let Some(imm) = number.cast() {
                    self.push(Instruction::Loadf32 { dst, imm })?
                } else {
                    self.push(Instruction::Loadf64 { dst, imm: number.0 })?
                };
                Ok(ExprResult::InstrDst(instr))
            }
            ast::PrimeExpr::String(_) => to_do!(),
            ast::PrimeExpr::Template(_) => to_do!(),
            ast::PrimeExpr::Regex(_) => to_do!(),
            ast::PrimeExpr::Ident(sym) => {
                let sym_id = self.variables.symbol_of_ast(sym);
                let use_order = dbg!(self.variables.use_of_ast(sym));
                let res = if let Some(reg) = self.registers.find_symbol(sym_id) {
                    ExprResult::Register(reg)
                } else {
                    let instr = self.push(Instruction::LoadPrim { dst, imm: 10 })?;
                    ExprResult::InstrDst(instr)
                };
                self.registers.advance_usage(use_order);
                Ok(res)
            }
            ast::PrimeExpr::Boolean(x) => {
                let instr = if x {
                    self.push(Instruction::LoadPrim { dst, imm: 6 })?
                } else {
                    self.push(Instruction::LoadPrim { dst, imm: 7 })?
                };
                Ok(ExprResult::InstrDst(instr))
            }
            ast::PrimeExpr::Function(_) => to_do!(),
            ast::PrimeExpr::Class(_) => to_do!(),
            ast::PrimeExpr::Object(_) => to_do!(),
            ast::PrimeExpr::Array(_) => to_do!(),
            ast::PrimeExpr::NewTarget | ast::PrimeExpr::This => {
                Ok(ExprResult::Register(Reg::this_reg()))
            }
            ast::PrimeExpr::Null => self
                .push(Instruction::LoadPrim { dst, imm: 2 })
                .map(ExprResult::InstrDst),
            ast::PrimeExpr::Super => to_do!(),
            ast::PrimeExpr::Covered(x) => self.compile_exprs(x),
        }
    }
}
