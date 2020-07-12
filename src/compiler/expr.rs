use crate::{
    ast::{AssignExpr, BinOp, Expr, Literal, Number, PrimeExpr},
    compiler::{Compiler, Result},
    runtime::bc::{self, DataValue, Op},
};

impl Compiler {
    pub fn parse_expression(&mut self, expr: &Expr) -> Result<Option<u8>> {
        let mut reg = None;
        for expr in expr.exprs.iter() {
            if let Some(x) = reg {
                self.regs.free(x);
            }
            reg = self.compile_assignment_expr(expr)?;
        }
        Ok(reg)
    }

    pub fn compile_assignment_expr(&mut self, expr: &AssignExpr) -> Result<Option<u8>> {
        match expr {
            AssignExpr::Prime(ref x) => Ok(Some(self.compile_prime_expr(x)?)),
            AssignExpr::Bin {
                ref lhs,
                op,
                ref rhs,
            } => {
                let left = self.compile_assignment_expr(lhs)?.unwrap();
                let right = self.compile_assignment_expr(rhs)?.unwrap();
                let op = match op {
                    BinOp::Plus => Op::ADD,
                    BinOp::Minus => Op::SUB,
                    BinOp::Mul => Op::MUL,
                    BinOp::Modulo => Op::MOD,
                    BinOp::Power => Op::POW,
                    _ => todo!(),
                };
                self.type_a(op, left, left, right);
                self.regs.free(right);
                Ok(Some(left))
            }
            _ => todo!(),
        }
    }

    pub fn compile_prime_expr(&mut self, expr: &PrimeExpr) -> Result<u8> {
        match expr {
            PrimeExpr::Null => {
                let reg = self.regs.alloc().unwrap();
                self.type_d(Op::CLP, reg, bc::PRIM_VAL_NULL);
                Ok(reg)
            }
            PrimeExpr::Literal(ref x) => {
                let reg = self.regs.alloc().unwrap();
                self.compile_literal(reg, x);
                Ok(reg)
            }
            PrimeExpr::ParamList { ref expr, ref rest } => {
                if rest.is_some() {
                    todo!()
                }
                if let Some(ref x) = expr {
                    Ok(self
                        .parse_expression(x)?
                        .expect("should be true, handle with error??"))
                } else {
                    todo!()
                }
            }
            PrimeExpr::Ident(ref x) => {
                let reg = self.regs.alloc().unwrap();
                let k_reg = self.regs.alloc().unwrap();
                self.load_data_value(k_reg, DataValue::String(x.0.clone()));
                self.type_a(Op::OGET, reg, 0xff, k_reg);
                self.regs.free(k_reg);
                Ok(reg)
            }
            _ => todo!(),
        }
    }

    pub fn compile_literal(&mut self, reg: u8, literal: &Literal) {
        match literal {
            Literal::Number(x) => match x {
                Number::Float(x) => {
                    let x = x.to_bits();
                    self.type_d(Op::CLF, reg, 0);
                    self.instructions.push(x as u32);
                    self.instructions.push((x >> 32) as u32);
                }
                Number::Integer(x) => {
                    self.load_int(reg, *x as u64);
                }
                Number::Big(_) => todo!(),
            },
            Literal::String(ref x) => self.load_data_value(reg, DataValue::String(x.clone())),
        }
    }
}
