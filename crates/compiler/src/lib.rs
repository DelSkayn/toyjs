#![allow(unused_variables)]

use ast::{Ast, ListHead};
use bc::{ByteCode, Instruction};
use common::{id, structs::Interners};
use core::fmt;
use registers::Registers;
use std::{backtrace::Backtrace, result::Result as StdResult};
use variables::Variables;

macro_rules! to_do {
    () => {
        return Err($crate::Error::NotImplemented(
            std::backtrace::Backtrace::capture(),
        ))
    };
}

mod expr;
mod prime;
mod proc;
mod registers;
mod stmt;
mod variables;

id!(pub struct InstructionId(u32));

pub type Result<T> = StdResult<T, Error>;
#[derive(Debug)]
pub enum Error {
    ExceededLimits,
    NotImplemented(Backtrace),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::ExceededLimits => write!(f, "Code exceeded interpreter limits"),
            Error::NotImplemented(b) => {
                write!(f, "Compiler hit path which was not yet implemented:\n{b}")
            }
        }
    }
}

pub struct Compiler<'a> {
    instructions: Vec<Instruction>,
    interners: &'a mut Interners,
    ast: &'a mut Ast,
    variables: Variables,
    registers: Registers,
}

impl<'a> Compiler<'a> {
    pub fn new(interners: &'a mut Interners, ast: &'a mut Ast) -> Self {
        Self {
            instructions: Vec::new(),
            interners,
            ast,
            variables: Variables::new(),
            registers: Registers::new(),
        }
    }

    pub fn push(&mut self, instr: Instruction) -> Result<InstructionId> {
        let res = self.instructions.len();
        let id = InstructionId(res.try_into().map_err(|_| Error::ExceededLimits)?);
        self.instructions.push(instr);
        Ok(id)
    }

    pub fn compile_script(mut self, script: ListHead<ast::Stmt>) -> Result<ByteCode> {
        self.resolve_variables(script)?;

        let mut expr = None;
        if let ListHead::Present(s) = script {
            let mut s = Some(s);
            while let Some(stmt_item) = s {
                let stmt = &self.ast[stmt_item];
                s = stmt.next;
                expr = self.compile_stmt(stmt.item)?;
            }
        }

        if let Some(x) = expr {
            self.push(Instruction::Ret { src: x })?;
        } else {
            self.push(Instruction::RetUndefind {})?;
        }

        self.into_bc()
    }
}
