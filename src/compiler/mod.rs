use crate::{
    ast::*,
    runtime::{
        bc::{self, op, Bytecode, DataValue, Instruction},
        string::StringRc,
        JSValue,
    },
};
use fxhash::FxHashMap;
use std::{result::Result as StdResult, u64};

#[macro_use]
mod macros;

mod error;
pub use error::{CompilerError, CompilerErrorKind};
mod expr;
mod stmt;

type Result<T> = StdResult<T, error::CompilerError>;

pub struct RegAlloc {
    regs: u64,
}

impl RegAlloc {
    pub fn new() -> Self {
        RegAlloc { regs: u64::MAX }
    }

    pub fn alloc(&mut self) -> Option<u8> {
        let next = self.regs.trailing_zeros();
        if next == 64 {
            return None;
        }
        let mask = 1 << next;
        self.regs ^= mask;
        return Some(next as u8);
    }

    pub fn free(&mut self, reg: u8) {
        let mask = 1 << reg as u64;
        self.regs |= mask
    }
}

pub struct Compiler {
    instructions: Vec<Instruction>,
    data: Vec<DataValue>,
    regs: RegAlloc,
    sym_table: FxHashMap<String, u8>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Vec::new(),
            data: Vec::new(),
            regs: RegAlloc::new(),
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

    pub fn compile_script(mut self, script: &Script) -> Result<Bytecode> {
        for stmt in script.stmts.iter() {
            self.compile_statement(stmt)?;
        }
        self.type_d(op::RET, 0, 0);
        Ok(Bytecode {
            instructions: self.instructions.into_boxed_slice(),
            data: self.data.into_boxed_slice(),
        })
    }

    pub fn load_int(&mut self, reg: u8, int: u64) {
        if int as u32 as u64 == int {
            let int = int as u32;
            self.type_d(op::CLL, reg, int as u16);
            if int & 0xffff_0000 != 0 {
                self.type_d(op::CLH, reg, (int >> 16) as u16);
            }
        } else {
            let v = (int as f64).to_bits();
            self.type_d(op::CLF, reg, 0);
            self.write(v as u32);
            self.write((v >> 32) as u32);
        }
    }
}
