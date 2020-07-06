use crate::{
    ast::{AssignExpr, BinOp, Expr, Literal, Number, PrimeExpr},
    compiler::{Compiler, Result},
    runtime::bc::{self, op, DataValue},
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
                    BinOp::Plus => op::ADD,
                    BinOp::Minus => op::SUB,
                    BinOp::Mul => op::MUL,
                    BinOp::Modulo => op::MOD,
                    BinOp::Power => op::POW,
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
                self.type_d(op::CLP, reg, bc::PRIM_VAL_NULL);
                Ok(reg)
            }
            PrimeExpr::Literal(ref x) => {
                let reg = self.regs.alloc().unwrap();
                self.compile_literal(reg, x);
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
                    self.type_d(op::CLF, reg, 0);
                    self.instructions.push(x as u32);
                    self.instructions.push((x >> 32) as u32);
                }
                Number::Integer(x) => {
                    self.load_int(reg, *x as u64);
                }
                Number::Big(_) => todo!(),
            },
            Literal::String(x) => {
                let idx = self.data.len();
                self.data.push(DataValue::String(x.clone()));
                if idx < 0x7fff {
                    self.type_d(op::CLD, reg, idx as u16);
                    return;
                }
                let tmp = self.regs.alloc().unwrap();
                self.load_int(tmp, idx as u64);
                self.type_d(op::CLD, reg, 0x8000 | tmp as u16);
                self.regs.free(tmp);
            }
        }
    }
}
