use crate::{
    ast::{AssignExpr, BinOp, Expr, Literal, Number, PrefixOp, PrimeExpr},
    compiler::{Compiler, Result},
    runtime::bc::{self, DataValue, Op},
};

impl Compiler {
    pub fn parse_expression(&mut self, expr: &Expr) -> Result<u8> {
        if expr.exprs.len() == 0 {
            todo!()
        }
        let mut reg = 0xff;
        for expr in expr.exprs.iter() {
            if reg != 0xff {
                self.regs.free(reg);
            }
            reg = self.compile_assignment_expr(expr)?;
        }
        Ok(reg)
    }

    pub fn compile_assignment_expr(&mut self, expr: &AssignExpr) -> Result<u8> {
        match expr {
            AssignExpr::Prime(ref x) => Ok(self.compile_prime_expr(x)?),
            AssignExpr::Prefix { ref op, ref expr } => {
                let reg = self.compile_assignment_expr(expr)?;
                match op {
                    PrefixOp::Increment => todo!(),
                    PrefixOp::Decrement => todo!(),
                    PrefixOp::Negative => {
                        self.type_d(Op::NEG, reg, reg as u16);
                        Ok(reg)
                    }
                    PrefixOp::Positive => {
                        self.type_d(Op::POS, reg, reg as u16);
                        Ok(reg)
                    }
                    _ => todo!(),
                }
            }
            AssignExpr::Bin {
                ref lhs,
                op,
                ref rhs,
            } => {
                let left = self.compile_assignment_expr(lhs)?;
                let right = self.compile_assignment_expr(rhs)?;
                let op = match op {
                    BinOp::Plus => Op::ADD,
                    BinOp::Minus => Op::SUB,
                    BinOp::Mul => Op::MUL,
                    BinOp::Modulo => Op::MOD,
                    BinOp::Power => Op::POW,
                    BinOp::ShiftLeft => Op::SHL,
                    BinOp::ShiftRight => Op::SHR,
                    BinOp::UnsignedShiftRight => Op::USR,
                    BinOp::BitwiseOr => Op::BOR,
                    BinOp::BitwiseAnd => Op::BAND,
                    BinOp::BitwiseXor => Op::BXOR,
                    _ => todo!(),
                };
                self.type_a(op, left, left, right);
                self.regs.free(right);
                Ok(left)
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
                    Ok(self.parse_expression(x)?)
                } else {
                    todo!()
                }
            }
            PrimeExpr::Boolean(x) => {
                let reg = self.regs.alloc().unwrap();
                let p = if *x {
                    bc::PRIM_VAL_TRUE
                } else {
                    bc::PRIM_VAL_FALSE
                };
                self.type_d(Op::CLP, reg, p);
                Ok(reg)
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
