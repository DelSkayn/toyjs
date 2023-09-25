#![allow(unused_variables)]
#![allow(dead_code)]

use ast::{Ast, ListHead};
use bc::{ByteCode, Instruction};
use common::{
    key, result::ContextError, source::Source, span::Span, string::Ascii, structs::Interners,
};
use core::fmt;
use registers::Registers;
use std::{backtrace::Backtrace, result::Result as StdResult};
use variables::{Variables, VariablesBuilder};

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
pub mod variables;

key!(pub struct InstructionId(u32));

pub type Result<T> = StdResult<T, Error>;
#[derive(Debug)]
pub enum Error {
    ExceededLimits(Limits),
    NotImplemented(Backtrace),
    Redeclared { span: Span, first_declared: Span },
}

#[derive(Debug)]
pub enum Limits {
    InstructionCount,
    JumpOffset,
    TooManyScopes,
    TooManyVariables,
}

impl fmt::Display for Limits {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Limits::InstructionCount => write!(f, "Too many instructions"),
            Limits::JumpOffset => {
                write!(f, "Tried to generate jump which exceeded max jump offset")
            }
            Limits::TooManyScopes => write!(f, "Exceeded the limit of variable scopes count"),
            Limits::TooManyVariables => write!(f, "Exceeded the limit of variable count"),
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
        let id = InstructionId(
            res.try_into()
                .map_err(|_| Error::ExceededLimits(Limits::InstructionCount))?,
        );
        self.instructions.push(instr);
        Ok(id)
    }

    pub fn compile_script(mut self, strict: bool, stmt: ListHead<ast::Stmt>) -> Result<ByteCode> {
        let mut variables = VariablesBuilder::new(self.ast);
        variables
            .push_scope(variables::ScopeKind::Global { strict })
            .unwrap();
        variables.resolve_variables(stmt)?;
        variables.pop_scope()?;

        self.variables = variables.build();

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
            self.push(Instruction::RetUndefind {})?;
        }

        self.into_bc()
    }
}
