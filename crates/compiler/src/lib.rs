#![allow(unused_variables)]

use ast::{Ast, ListHead, NodeId};
use bc::{ByteCode, Instruction, Reg};
use common::{id, number::Number, structs::Interners};
use std::result::Result as StdResult;

macro_rules! to_do {
    () => {
        return Err($crate::Error::NotImplemented)
    };
}

mod expr;
mod prime;
mod proc;
mod stmt;

id!(pub struct InstructionId(u32));

pub type Result<T> = StdResult<T, Error>;
pub enum Error {
    ExceededLimits,
    NotImplemented,
}

pub struct Compiler<'a> {
    instructions: Vec<Instruction>,
    register: i8,
    interners: &'a mut Interners,
    ast: &'a mut Ast,
}

impl<'a> Compiler<'a> {
    pub fn new(interners: &'a mut Interners, ast: &'a mut Ast) -> Self {
        Self {
            instructions: Vec::new(),
            register: 0,
            interners,
            ast,
        }
    }

    pub fn push(&mut self, instr: Instruction) -> Result<InstructionId> {
        let res = self.instructions.len();
        let id = InstructionId(res.try_into().map_err(|_| Error::ExceededLimits)?);
        self.instructions.push(instr);
        Ok(id)
    }

    pub fn compile_script(mut self, script: ListHead<ast::Stmt>) -> Result<ByteCode> {
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
            self.push(Instruction::Ret { src: Reg(x) })?;
        } else {
            self.push(Instruction::RetUndefind {})?;
        }

        self.into_bc()
    }
}
