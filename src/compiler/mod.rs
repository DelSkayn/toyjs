use crate::ast::*;
use crate::runtime::{
    bc::{self, op, Bytecode, Instruction},
    string::StringRc,
    JSValue,
};

pub struct Compiler {
    instructions: Vec<Instruction>,
    data: Vec<JSValue>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Vec::new(),
            data: Vec::new(),
        }
    }

    #[inline]
    pub fn type_a(&mut self, op: u8, a: u8, b: u8, c: u8) {
        self.instructions.push(bc::type_a(op, a, b, c))
    }

    #[inline]
    pub fn type_d(&mut self, op: u8, a: u8, d: u16) {
        self.instructions.push(bc::type_d(op, a, d))
    }

    #[inline]
    pub fn write(&mut self, instr: Instruction) {
        self.instructions.push(instr)
    }

    pub fn compile_script(&mut self, script: Script) -> Result<Bytecode, ()> {
        for stmt in script.stmts.iter() {
            self.compile_statement(stmt)?;
        }
        self.type_d(op::RET, 0, 0);
        Ok(Bytecode {
            instructions: self.instructions.clone().into_boxed_slice(),
            data: self.data.clone().into_boxed_slice(),
        })
    }

    pub fn compile_statement(&mut self, stmt: &Stmt) -> Result<(), ()> {
        match *stmt {
            Stmt::Expr { ref expr } => self.compile_expr(expr),
            _ => Err(()),
        }
    }

    pub fn compile_expr(&mut self, expr: &Expr) -> Result<(), ()> {
        for expr in expr.exprs.iter() {
            self.compile_assign_expr(expr, &mut 0)?;
        }
        Ok(())
    }

    pub fn compile_assign_expr(&mut self, op: &AssignExpr, free_reg: &mut u8) -> Result<u8, ()> {
        match *op {
            AssignExpr::Bin {
                ref lhs,
                ref op,
                ref rhs,
            } => {
                let rl = self.compile_assign_expr(lhs, free_reg)?;
                let rr = self.compile_assign_expr(rhs, free_reg)?;
                *free_reg -= 1;
                match op {
                    BinOp::Plus => {
                        self.type_a(op::ADD, rl, rl, rr);
                        return Ok(rl);
                    }
                    BinOp::Minus => {
                        self.type_a(op::SUB, rl, rl, rr);
                        return Ok(rl);
                    }
                    BinOp::Mul => {
                        self.type_a(op::MUL, rl, rl, rr);
                        return Ok(rl);
                    }
                    BinOp::Div => {
                        self.type_a(op::DIV, rl, rl, rr);
                        return Ok(rl);
                    }
                    BinOp::Modulo => {
                        self.type_a(op::MOD, rl, rl, rr);
                        return Ok(rl);
                    }
                    BinOp::Power => {
                        self.type_a(op::POW, rl, rl, rr);
                        return Ok(rl);
                    }
                    _ => return Err(()),
                }
            }
            AssignExpr::Prime(ref p) => return self.compile_prime_expr(p, free_reg),
            _ => return Err(()),
        }
    }

    pub fn compile_prime_expr(&mut self, prime: &PrimeExpr, free_reg: &mut u8) -> Result<u8, ()> {
        match *prime {
            PrimeExpr::Boolean(value) => {
                let cur = *free_reg;
                *free_reg += 1;
                let val = if value {
                    bc::PRIM_VAL_TRUE
                } else {
                    bc::PRIM_VAL_FALSE
                };
                self.type_d(op::CLP, cur, val);
                return Ok(cur);
            }
            PrimeExpr::Literal(ref x) => self.compile_literal(x, free_reg),
            PrimeExpr::Null => {
                let cur = *free_reg;
                *free_reg += 1;
                self.type_d(op::CLP, cur, bc::PRIM_VAL_NULL);
                return Ok(cur);
            }
            _ => Err(()),
        }
    }

    pub fn compile_literal(&mut self, lit: &Literal, free_reg: &mut u8) -> Result<u8, ()> {
        match lit {
            Literal::String(ref x) => {
                let cur = *free_reg;
                *free_reg += 1;
                let data_value = StringRc::new(x.clone());
                let js_value = unsafe { JSValue::string(data_value) };
                let idx = self.data.len() as u64;
                self.data.push(js_value);
                // does it fit
                if idx < 0x7fff {
                    self.type_d(op::CLD, cur, idx as u16);
                } else {
                    self.load_integer(cur, idx);
                    let val = cur as u16 | 0x8000;
                    self.type_d(op::CLD, cur, val);
                }
                return Ok(cur);
            }
            Literal::Number(Number::Integer(x)) => {
                let x = *x as u32;
                let cur = *free_reg;
                *free_reg += 1;
                self.load_integer(cur, x as u64);
                return Ok(cur);
            }
            Literal::Number(Number::Float(x)) => {
                let cur = *free_reg;
                *free_reg += 1;
                self.type_d(op::CLF, cur, 0);
                let val = x.to_bits();
                self.write((val & 0xffff_ffff) as u32);
                self.write((val >> 32) as u32);
                return Ok(cur);
            }
            Literal::Number(Number::Big(_)) => {
                return Err(());
            }
        }
    }

    pub fn load_integer(&mut self, reg: u8, int: u64) {
        if int < 0xffff_ffff {
            self.type_d(op::CLL, reg, (int & 0xffff) as u16);
            let rem = int >> 16;
            if rem != 0 {
                self.type_d(op::CLH, reg, rem as u16);
            }
            return;
        }
        let val = int as f64;
        if val as u64 != int {
            // TODO replace with error?
            panic!("data size grew to large to fit into 52 bit float")
        }
        self.type_d(op::CLF, reg, 0);
        let val = val.to_bits();
        self.write((val & 0xffff_ffff) as u32);
        self.write((val >> 32) as u32);
    }
}
