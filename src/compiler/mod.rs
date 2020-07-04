use crate::{
    ast::*,
    runtime::{
        bc::{self, op, Bytecode, Instruction},
        string::StringRc,
        JSValue,
    },
};
use fxhash::FxHashMap;

pub struct Compiler {
    instructions: Vec<Instruction>,
    data: Vec<JSValue>,
    free_reg: u8,
    sym_table: FxHashMap<String, u8>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Vec::new(),
            data: Vec::new(),
            free_reg: 0,
            sym_table: FxHashMap::default(),
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
            Stmt::Declaration { ref kind } => match kind {
                DeclKind::Lexical(ref x) => {
                    for d in x.decl.iter() {
                        self.compile_lexical_decl(d)?;
                    }
                    Ok(())
                }
                _ => Err(()),
            },
            _ => Err(()),
        }
    }

    pub fn compile_lexical_decl(&mut self, decl: &LexicalDecl) -> Result<(), ()> {
        let tok = match decl.binding {
            Binding::Ident(ref x) => x,
            _ => return Err(()),
        };
        if let Some(reg) = self.sym_table.get(&tok.0).cloned() {
            if let Some(ref x) = decl.initializer {
                let expr_reg = self.compile_assign_expr(x)?;
                self.type_d(op::MOV, reg, expr_reg as u16);
                self.free_reg -= 1;
            }
            return Ok(());
        }
        let reg = if let Some(ref x) = decl.initializer {
            self.compile_assign_expr(x)?
        } else {
            let res = self.free_reg;
            self.free_reg += 1;
            res
        };
        self.sym_table.insert(tok.0.clone(), reg);
        Ok(())
    }

    pub fn compile_expr(&mut self, expr: &Expr) -> Result<(), ()> {
        for expr in expr.exprs.iter() {
            self.compile_assign_expr(expr)?;
        }
        Ok(())
    }

    pub fn compile_assign_expr(&mut self, op: &AssignExpr) -> Result<u8, ()> {
        match *op {
            AssignExpr::Bin {
                ref lhs,
                ref op,
                ref rhs,
            } => {
                let rl = self.compile_assign_expr(lhs)?;
                let rr = self.compile_assign_expr(rhs)?;
                self.free_reg -= 1;
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
            AssignExpr::Prime(ref p) => return self.compile_prime_expr(p),
            _ => return Err(()),
        }
    }

    pub fn compile_prime_expr(&mut self, prime: &PrimeExpr) -> Result<u8, ()> {
        match *prime {
            PrimeExpr::Boolean(value) => {
                let cur = self.free_reg;
                self.free_reg += 1;
                let val = if value {
                    bc::PRIM_VAL_TRUE
                } else {
                    bc::PRIM_VAL_FALSE
                };
                self.type_d(op::CLP, cur, val);
                return Ok(cur);
            }
            PrimeExpr::Literal(ref x) => self.compile_literal(x),
            PrimeExpr::Null => {
                let cur = self.free_reg;
                self.free_reg += 1;
                self.type_d(op::CLP, cur, bc::PRIM_VAL_NULL);
                return Ok(cur);
            }
            PrimeExpr::Ident(ref x) => {
                let reg = self.sym_table.get(&x.0).cloned().ok_or(())?;
                let cur = self.free_reg;
                self.free_reg += 1;
                self.type_d(op::MOV, cur, reg as u16);
                return Ok(cur);
            }
            _ => Err(()),
        }
    }

    pub fn compile_literal(&mut self, lit: &Literal) -> Result<u8, ()> {
        match lit {
            Literal::String(ref x) => {
                let cur = self.free_reg;
                self.free_reg += 1;
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
                let cur = self.free_reg;
                self.free_reg += 1;
                self.load_integer(cur, x as u64);
                return Ok(cur);
            }
            Literal::Number(Number::Float(x)) => {
                let cur = self.free_reg;
                self.free_reg += 1;
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
