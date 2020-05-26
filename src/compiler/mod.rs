use crate::ast::*;
use crate::runtime::bytecode::{Bytecode, BytecodeBuilder};
use crate::runtime::JSValue;

pub struct Compiler {
    code: BytecodeBuilder,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            code: BytecodeBuilder::new(),
        }
    }

    pub fn compile_script(&mut self, script: Script) -> Result<Bytecode, ()> {
        for stmt in script.stmts.iter() {
            self.compile_statement(stmt)?;
        }
        self.code.RET();
        Ok(self.code.clone().build())
    }

    pub fn compile_statement(&mut self, stmt: &Stmt) -> Result<(), ()> {
        match *stmt {
            Stmt::Expr { ref expr } => self.compile_expr(expr),
            _ => Err(()),
        }
    }

    pub fn compile_expr(&mut self, expr: &Expr) -> Result<(), ()> {
        for expr in expr.exprs.iter() {
            self.compile_assign_expr(expr)?;
        }
        Ok(())
    }

    pub fn compile_assign_expr(&mut self, op: &AssignExpr) -> Result<(), ()> {
        match *op {
            AssignExpr::Bin {
                ref lhs,
                ref op,
                ref rhs,
            } => {
                self.compile_assign_expr(lhs)?;
                self.compile_assign_expr(rhs)?;
                match op {
                    BinOp::Plus => {
                        self.code.ADD();
                    }
                    BinOp::Minus => {
                        self.code.ADD();
                    }
                    BinOp::Mul => {
                        self.code.MUL();
                    }
                    BinOp::Div => {
                        self.code.DIV();
                    }
                    BinOp::Modulo => {
                        self.code.MOD();
                    }
                    BinOp::Power => {
                        self.code.POW();
                    }
                    _ => return Err(()),
                }
            }
            AssignExpr::Prime(ref p) => self.compile_prime_expr(p)?,
            _ => return Err(()),
        }
        Ok(())
    }

    pub fn compile_prime_expr(&mut self, prime: &PrimeExpr) -> Result<(), ()> {
        match *prime {
            PrimeExpr::Literal(Literal::Number(Number::Integer(x))) => {
                self.code.LD_INT(x as i32);
                return Ok(());
            }
            PrimeExpr::Literal(Literal::Number(Number::Float(x))) => {
                self.code.LD_VAL(JSValue::float(x));
                return Ok(());
            }
            _ => Err(()),
        }
    }
}
