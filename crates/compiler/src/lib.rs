#![allow(unused_variables)]
#![allow(dead_code)]

use core::fmt;
use std::{backtrace::Backtrace, collections::VecDeque, result::Result as StdResult, u8};

use ast::{Ast, ListHead, NodeId};
use bc::{ByteCode, Instruction, InstructionType, LongOffset, OpCode, Reg};
use common::{
    hashmap::HashMap,
    key,
    result::ContextError,
    source::Source,
    span::Span,
    string::{Ascii, String, StringId},
    structs::Interners,
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
    Functions,
    Strings,
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
            Limits::Functions => write!(f, "number of functions in script exceeded limit"),
            Limits::Strings => write!(f, "number of strings in script exceeded limit"),
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
                writeln!(
                    f,
                    "{}",
                    ctx.render_span(*span, None)
                        .map_err(|_| fmt::Error)?
                        .as_block()
                )?;
                writeln!(
                    f,
                    "{}",
                    ctx.render_span(
                        *first_declared,
                        Some(Ascii::const_from_str("Was first declared here").into()),
                    )
                    .map_err(|_| fmt::Error)?
                    .as_block()
                )
            }
        }
    }
}

struct PendingArg {
    instruction: InstrOffset,
    offset: u16,
}

pub struct Compiler<'a> {
    // static structures used for compiling
    ast: &'a mut Ast,
    interners: &'a mut Interners,
    variables: Variables,

    // functions we still need to compile.
    pending_functions: VecDeque<NodeId<ast::Function>>,
    next_function_id: u32,

    // data created during compilation
    functions: Vec<bc::Function>,
    instructions: Vec<u8>,

    // data structures storing information about the current function being compiled.
    /// Data about the current register allocation
    registers: Registers,
    /// A list of instructions which push argument onto the stack which need to be patched later
    /// once we found how many registers the current function requreis
    arg_patch: Vec<PendingArg>,
    max_arg: u16,
    /// Strings which were referenced in compilation along with the id assigned to them
    strings: HashMap<StringId, u32>,
    /// The id for the next string.
    next_string_id: u32,
}

impl<'a> Compiler<'a> {
    pub fn new(interners: &'a mut Interners, ast: &'a mut Ast) -> Self {
        Self {
            interners,
            ast,
            variables: Variables::new(),

            pending_functions: VecDeque::new(),
            next_function_id: 0,

            functions: Vec::new(),
            instructions: Vec::new(),

            registers: Registers::new(),
            arg_patch: Vec::new(),
            max_arg: 0,
            strings: HashMap::default(),
            next_string_id: 0,
        }
    }

    fn reset_for_function(&mut self) {
        debug_assert!(self.arg_patch.is_empty());
        self.instructions.clear();
        self.max_arg = 0;
    }

    fn push_arg(&mut self, instr: InstrOffset, offset: u16) {
        self.arg_patch.push(PendingArg {
            instruction: instr,
            offset,
        });
        self.max_arg = self.max_arg.max(offset);
    }

    #[inline(always)]
    fn emit(&mut self, instr: Instruction) -> Result<InstrOffset> {
        let res = self.instructions.len();
        let id = InstrOffset(
            res.try_into()
                .map_err(|_| Error::ExceededLimits(Limits::BytecodeSize))?,
        );
        instr.write(&mut self.instructions);
        Ok(id)
    }

    fn next_instruction(&mut self) -> Result<InstrOffset> {
        let res = self.instructions.len();
        let id = InstrOffset(
            res.try_into()
                .map_err(|_| Error::ExceededLimits(Limits::BytecodeSize))?,
        );
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

    pub fn map_string(&mut self, id: StringId) -> u32 {
        *self.strings.entry(id).or_insert_with(|| {
            let res = self.next_string_id;
            self.next_string_id += 1;
            res
        })
    }

    pub fn compile_script(mut self, strict: bool, stmt: ListHead<ast::Stmt>) -> Result<ByteCode> {
        let root_scope = self.variables.push_global_scope(strict);
        self.next_function_id += 1;

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
            self.emit(Instruction::Ret { src: x })?;
        } else {
            self.emit(Instruction::RetUndefined {})?;
        }

        self.finalize_instructions(0)?;

        let mut before = self.instructions.len();

        while let Some(func) = self.pending_functions.pop_front() {
            self.registers.clear();
            self.compile_function(func)?;
            self.finalize_instructions(before as u32)?;
            before = self.instructions.len();
        }

        let mut strings = vec![String::new(); self.next_string_id as usize];
        for (id, k) in self.strings {
            strings[k as usize] = self.interners.strings.get(id).unwrap().clone();
        }

        Ok(ByteCode {
            functions: self.functions.into_boxed_slice(),
            strings: strings.into_boxed_slice(),
            instructions: self.instructions.into_boxed_slice(),
        })
    }

    pub fn compile_function(&mut self, ast: NodeId<ast::Function>) -> Result<()> {
        /*
        let
            ast::Function::Arrow { kind, params, rest_param, body, .. } |
            ast::Function::Declared {  kind,  params, rest_param, body, .. } |
            ast::Function::Expr {  kind,  params, rest_param, body, .. }  = self.ast[ast];

        match kind{
            ast::FunctionKind::Async | ast::FunctionKind::Generator | ast::FunctionKind::AsyncGenerator => to_do!(),
            ast::FunctionKind::Simple => {},
        }

        let _params = params;
        let _rest_param = rest_param;

        if let ListHead::Present(mut cur) = body{
            loop{
                self.compile_stmt(self.ast[cur].item)?;
                let Some(next) = self.ast[cur].next else{
                    break
                };
                cur = next;
            }
        }
        */

        Ok(())
    }
}
