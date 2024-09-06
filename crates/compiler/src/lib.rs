#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(clippy::new_without_default)]

use ast::{Ast, NodeId, NodeListId};
use bc::{ByteCode, Instruction, InstructionType, LongOffset, OpCode, Reg};
use bytemuck::NoUninit;
use common::{hashmap::hash_map::HashMap, id, string::String};

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
mod stmt;

mod error;
pub use error::{Error, Limits, Result};
mod function;
use function::PendingFunctionList;
mod symbol;
use symbol::RegisterInfo;
pub mod variables;
use variables::{resolve_script, SymbolId, Variables};

id!(
    /// Offset into the instruction buffer in bytes.
    pub struct InstrOffset
);

/// An argument whose location is an offset from the end of the function frame.
/// The size of the frame is only found after all instructions are generated so therefore assigning
/// these argument an destination
struct PendingArg {
    instruction: InstrOffset,
    offset: u16,
}

struct TempLoadedSymbol {
    symbol: SymbolId,
    register: Reg,
}

pub struct Compiler<'a> {
    // static structures used for compiling
    ast: &'a mut Ast,
    variables: Variables,

    // functions we still need to compile.
    pending_functions: PendingFunctionList,

    // data created during compilation
    functions: Vec<bc::Function>,
    instructions: Vec<u8>,

    /// A list of instructions which push argument onto the stack which need to be patched later
    /// once we found how many registers the current function requreis
    arg_patch: Vec<PendingArg>,
    max_arg: u16,
    /// Strings which were referenced in compilation along with the id assigned to them
    strings: HashMap<NodeId<String>, u32>,
    /// The id for the next string.
    next_string_id: u32,
    /// Data regarding register allocation and symbol placement.
    regs: RegisterInfo,
}

impl<'a> Compiler<'a> {
    pub fn new(ast: &'a mut Ast) -> Self {
        Self {
            ast,
            variables: Variables::new(),

            pending_functions: PendingFunctionList::new(),

            functions: Vec::new(),
            instructions: Vec::new(),

            arg_patch: Vec::new(),
            max_arg: 0,
            strings: HashMap::default(),
            next_string_id: 0,

            regs: RegisterInfo::new(),
        }
    }

    fn reset_for_function(&mut self) {
        self.arg_patch.clear();
        self.max_arg = 0;
        self.regs.reset();
    }

    fn push_arg(&mut self, instr: InstrOffset, offset: u16) {
        self.arg_patch.push(PendingArg {
            instruction: instr,
            offset,
        });
        self.max_arg = self.max_arg.max(offset);
    }

    fn push_pending_function(&mut self, func: NodeId<ast::Function>) -> bc::FunctionId {
        self.pending_functions.push(func)
    }

    #[inline(always)]
    fn emit(&mut self, instr: Instruction) -> Result<InstrOffset> {
        let res = self.instructions.len();
        let id = res
            .try_into()
            .ok()
            .and_then(InstrOffset::from_u32)
            .ok_or(Error::Limit(Limits::BytecodeSize))?;
        instr.write(&mut self.instructions);
        Ok(id)
    }

    pub fn push_instr_byte<B: NoUninit>(&mut self, b: B) -> Result<InstrOffset> {
        let res = self.instructions.len();
        let id = res
            .try_into()
            .ok()
            .and_then(InstrOffset::from_u32)
            .ok_or(Error::Limit(Limits::BytecodeSize))?;
        self.instructions.push(bytemuck::cast(b));
        Ok(id)
    }

    fn emit_move(&mut self, dst: Reg, src: Reg) -> Result<Option<InstrOffset>> {
        if dst == src {
            return Ok(None);
        }
        self.emit(Instruction::Move { dst, src }).map(Some)
    }

    fn next_instruction(&self) -> Result<InstrOffset> {
        let res = self.instructions.len();
        let id = res
            .try_into()
            .ok()
            .and_then(InstrOffset::from_u32)
            .ok_or(Error::Limit(Limits::BytecodeSize))?;
        Ok(id)
    }

    fn patch_dst(&mut self, instr: InstrOffset, to: Reg) {
        debug_assert!(
            OpCode::from_u8(self.instructions[instr.into_u32() as usize])
                .unwrap()
                .has_dst_register(),
            "tried to patch dst register of opcode {:?} with no dst register",
            OpCode::from_u8(self.instructions[instr.into_u32() as usize]).unwrap()
        );

        // dst registers are always the first register after the opcode.
        self.instructions[instr.into_u32() as usize + 1] = to.0 as u8;
    }

    /// Patch a jump instruction to jump to a give instruction.
    fn patch_jump(&mut self, instr: InstrOffset, to: InstrOffset) -> Result<()> {
        let opcode = OpCode::from_u8(self.instructions[instr.into_u32() as usize]).unwrap();
        match opcode {
            OpCode::LongJump | OpCode::TryLong => {
                let from_point = instr.into_u32() as i64 + bc::types::LongJump::SIZE as i64;
                let offset = LongOffset(
                    i32::try_from(to.into_u32() as i64 - from_point)
                        .map_err(|_| Error::Limit(Limits::JumpOffset))?,
                );
                let instr_start = instr.into_u32() as usize + 1;
                self.instructions[instr_start..(instr_start + std::mem::size_of::<LongOffset>())]
                    .copy_from_slice(bytemuck::bytes_of(&offset));
                Ok(())
            }
            OpCode::LongJumpTrue | OpCode::LongJumpFalse => {
                let from_point = instr.into_u32() as i64 + bc::types::LongJumpTrue::SIZE as i64;
                let offset = LongOffset(
                    i32::try_from(to.into_u32() as i64 - from_point)
                        .map_err(|_| Error::Limit(Limits::JumpOffset))?,
                );
                let instr_start = instr.into_u32() as usize + 2;
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

    pub fn map_string(&mut self, id: NodeId<String>) -> u32 {
        *self.strings.entry(id).or_insert_with(|| {
            let res = self.next_string_id;
            self.next_string_id += 1;
            res
        })
    }

    pub fn compile_script(
        mut self,
        strict: bool,
        stmt: Option<NodeListId<ast::Stmt>>,
    ) -> Result<ByteCode> {
        let root_scope = self.variables.push_global_scope(strict);

        // resolve all variables in the statement.
        if let Some(root) = stmt {
            resolve_script(root, self.ast, &mut self.variables, root_scope)?;
        }

        // compile the main function or the root of the script.
        let mut expr = None;
        if let Some(s) = stmt {
            let mut s = Some(s);
            while let Some(stmt_item) = s {
                let stmt = &self.ast[stmt_item];
                s = stmt.next;
                expr = self.compile_stmt(stmt.item)?;
            }
        }

        // Add a return instruction a the end.
        if let Some(x) = expr {
            self.emit(Instruction::Ret { src: x })?;
        } else {
            self.emit(Instruction::RetUndefined {})?;
        }

        // do finalizing operations now that instructions are finalized.
        self.finalize_instructions(0)?;

        // Keep track where the new function start
        let mut before = self.instructions.len();
        // Compile all functions
        while let Some(func) = self.pending_functions.pop_pending() {
            self.reset_for_function();
            self.compile_function(func)?;
            self.finalize_instructions(before as u32)?;
            before = self.instructions.len();
        }

        // Push all the used strings into a single slice.
        let mut strings = vec![String::new(); self.next_string_id as usize];
        for (id, k) in self.strings {
            strings[k as usize] = self.ast[id].clone();
        }

        Ok(ByteCode {
            functions: self.functions.into_boxed_slice(),
            strings: strings.into_boxed_slice(),
            instructions: self.instructions.into_boxed_slice(),
        })
    }

    pub fn compile_function(&mut self, func: NodeId<ast::Function>) -> Result<()> {
        let (kind, name, body) = match self.ast[func] {
            ast::Function::Arrow { .. } => to_do!(),
            ast::Function::Declared {
                kind, body, name, ..
            } => (kind, Some(name), body),
            ast::Function::Expr {
                kind, body, name, ..
            } => (kind, name, body),
        };

        match kind {
            ast::FunctionKind::Async
            | ast::FunctionKind::Generator
            | ast::FunctionKind::AsyncGenerator => to_do!(),
            ast::FunctionKind::Simple => {}
        }

        let expr = if let Some(mut cur) = body {
            let mut expr;
            loop {
                expr = self.compile_stmt(self.ast[cur].item)?;
                let Some(next) = self.ast[cur].next else {
                    break;
                };
                cur = next;
            }
            expr
        } else {
            None
        };

        // Add a return instruction a the end.
        if let Some(x) = expr {
            self.emit(Instruction::Ret { src: x })?;
        } else {
            self.emit(Instruction::RetUndefined {})?;
        }

        Ok(())
    }
}
