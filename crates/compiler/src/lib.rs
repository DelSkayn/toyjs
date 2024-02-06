#![allow(unused_variables)]
#![allow(dead_code)]

use core::fmt;
use std::{backtrace::Backtrace, result::Result as StdResult, u8};

use ast::{Ast, ListHead};
use bc::{ByteCode, Instruction, InstructionType, LongOffset, OpCode, Reg};
use common::{
    key, result::ContextError, source::Source, span::Span, string::Ascii, structs::Interners,
};
use registers::Registers;
use variables::{resolve_script, Variables};

macro_rules! to_do {
    () => {
        return Err($crate::Error::NotImplemented(
            std::backtrace::Backtrace::capture(),
        ))
    };
}

mod decl;
mod expr;
mod prime;
mod proc;
mod registers;
mod stmt;
pub mod variables;

key!(
    /// Offset into the instruction buffer in bytes.
    pub struct InstrOffset(u32)
);

pub type Result<T> = StdResult<T, Error>;
#[derive(Debug)]
pub enum Error {
    ExceededLimits(Limits),
    NotImplemented(Backtrace),
    Redeclared { span: Span, first_declared: Span },
}

#[derive(Debug)]
pub enum Limits {
    BytecodeSize,
    JumpOffset,
    TooManyScopes,
    TooManyVariables,
    Registers,
}

impl fmt::Display for Limits {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Limits::BytecodeSize => write!(
                f,
                "script required more then u32::MAX bytes of instructions"
            ),
            Limits::JumpOffset => {
                write!(f, "script required a jump which exceeded the maximum jump offset allowed by the instruction set.")
            }
            Limits::TooManyScopes => {
                write!(f, "script has more scopes than the limit of u32::MAX - 1")
            }
            Limits::TooManyVariables => {
                write!(f, "script has more symbols than the limit of u32::MAX - 1")
            }
            Limits::Registers => write!(
                f,
                "function in script required more registers then the instruction set limit of 127"
            ),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::ExceededLimits(limit) => {
                write!(f, "Code exceeded interpreter limits: {limit}")
            }
            Error::NotImplemented(b) => {
                write!(f, "Compiler hit path which was not yet implemented:\n{b}")
            }
            Error::Redeclared { .. } => {
                writeln!(f, "Already existing variable was redeclared")
            }
        }
    }
}

impl ContextError<Source> for Error {
    fn display(&self, f: &mut fmt::Formatter, ctx: &Source) -> fmt::Result {
        match self {
            Error::ExceededLimits(limit) => {
                write!(f, "Code exceeded interpreter limits: {limit}")
            }
            Error::NotImplemented(b) => {
                write!(f, "Compiler hit path which was not yet implemented:\n{b}")
            }
            Error::Redeclared {
                span,
                first_declared,
            } => {
                writeln!(f, "Already existing variable was redeclared")?;
                ctx.render_string_block(f, *span, None)
                    .map_err(|_| fmt::Error)?;
                writeln!(f)?;
                writeln!(f)?;
                ctx.render_string_block(
                    f,
                    *first_declared,
                    Some(Ascii::const_from_str("Was first declared here").into()),
                )
                .map_err(|_| fmt::Error)
            }
        }
    }
}

pub struct Compiler<'a> {
    instructions: Vec<u8>,
    interners: &'a mut Interners,
    ast: &'a mut Ast,
    registers: Registers,
    variables: Variables,
}

impl<'a> Compiler<'a> {
    pub fn new(interners: &'a mut Interners, ast: &'a mut Ast) -> Self {
        Self {
            instructions: Vec::new(),
            interners,
            ast,
            registers: Registers::new(),
            variables: Variables::new(),
        }
    }

    fn push(&mut self, instr: Instruction) -> Result<InstrOffset> {
        let res = self.instructions.len();
        let id = InstrOffset(
            res.try_into()
                .map_err(|_| Error::ExceededLimits(Limits::BytecodeSize))?,
        );
        instr.write(&mut self.instructions);
        Ok(id)
    }

    fn patch_dst(&mut self, instr: InstrOffset, to: Reg) {
        debug_assert!(
            OpCode::from_u8(self.instructions[instr.0 as usize])
                .unwrap()
                .has_dst_register(),
            "tried to patch dst register of opcode {:?} with no dst register",
            OpCode::from_u8(self.instructions[instr.0 as usize]).unwrap()
        );

        // dst registers are always the first register after the opcode.
        self.instructions[instr.0 as usize + 1] = to.0 as u8;
    }

    /// Patch a jump instruction to jump to a give instruction.
    fn patch_jump(&mut self, instr: InstrOffset, to: InstrOffset) -> Result<()> {
        let opcode = OpCode::from_u8(self.instructions[instr.0 as usize]).unwrap();
        match opcode {
            OpCode::LongJump | OpCode::TryLong => {
                let from_point = instr.0 as i64 + bc::types::LongJump::SIZE as i64;
                let offset = LongOffset(
                    i32::try_from(to.0 as i64 - from_point)
                        .map_err(|_| Error::ExceededLimits(Limits::JumpOffset))?,
                );
                let instr_start = instr.0 as usize + 1;
                self.instructions[instr_start..(instr_start + std::mem::size_of::<LongOffset>())]
                    .copy_from_slice(bytemuck::bytes_of(&offset));
                Ok(())
            }
            OpCode::LongJumpTrue | OpCode::LongJumpFalse => {
                let from_point = instr.0 as i64 + bc::types::LongJumpTrue::SIZE as i64;
                let offset = LongOffset(
                    i32::try_from(to.0 as i64 - from_point)
                        .map_err(|_| Error::ExceededLimits(Limits::JumpOffset))?,
                );
                let instr_start = instr.0 as usize + 2;
                self.instructions[instr_start..(instr_start + std::mem::size_of::<LongOffset>())]
                    .copy_from_slice(bytemuck::bytes_of(&offset));
                Ok(())
            }
            OpCode::Jump | OpCode::Try | OpCode::JumpTrue | OpCode::JumpFalse => {
                panic!("non long jumps should not be used during codegen")
            }
            _ => {
                panic!("tried to patch non jump")
            }
        }
    }

    pub fn compile_script(mut self, strict: bool, stmt: ListHead<ast::Stmt>) -> Result<ByteCode> {
        let root_scope = self.variables.push_global_scope(strict);

        if let ListHead::Present(root) = stmt {
            resolve_script(root, self.ast, &mut self.variables, root_scope)?;
        }

        self.registers = Registers::new();

        let mut expr = None;
        if let ListHead::Present(s) = stmt {
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
            self.push(Instruction::RetUndefined {})?;
        }

        self.into_bc()
    }
}
