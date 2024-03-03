#![allow(unused_variables)]
#![allow(dead_code)]

use std::{collections::VecDeque, u8};

use ast::{Ast, ListHead, NodeId};
use bc::{ByteCode, Instruction, InstructionType, LongOffset, OpCode, Reg};
use common::{
    hashmap::hash_map::HashMap,
    key,
    string::{String, StringId},
    structs::Interners,
};
use symbol::{Reservation, SymbolPlacement};
use use_bitmap::UseBitmap;
use variables::{resolve_script, SymbolId, SymbolUseOrder, Variables};

macro_rules! to_do {
    () => {
        return Err($crate::Error::NotImplemented(
            std::backtrace::Backtrace::capture(),
        ))
    };
}

mod decl;
mod error;
mod expr;
mod prime;
mod proc;
mod stmt;
mod symbol;
mod use_bitmap;
pub mod variables;

pub use error::{Error, Limits, Result};

key!(
    /// Offset into the instruction buffer in bytes.
    pub struct InstrOffset(u32)
);

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
    interners: &'a mut Interners,
    variables: Variables,

    // functions we still need to compile.
    pending_functions: VecDeque<NodeId<ast::Function>>,
    next_function_id: u32,

    // data created during compilation
    functions: Vec<bc::Function>,
    instructions: Vec<u8>,

    /// A list of instructions which push argument onto the stack which need to be patched later
    /// once we found how many registers the current function requreis
    arg_patch: Vec<PendingArg>,
    max_arg: u16,
    /// Strings which were referenced in compilation along with the id assigned to them
    strings: HashMap<StringId, u32>,
    /// The id for the next string.
    next_string_id: u32,
    // -- register allocation structs --
    function_stack_size: u32,
    used_registers: UseBitmap,
    tmp_registers: UseBitmap,
    tmp_symbol_registers: UseBitmap,
    reservations: Vec<Reservation>,
    inflight_symbols: HashMap<SymbolId, SymbolPlacement>,
    tmp_symbol_id: HashMap<Reg, SymbolId>,
    global_register: Option<Reg>,
    current_use_order: SymbolUseOrder,
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

            arg_patch: Vec::new(),
            max_arg: 0,
            strings: HashMap::default(),
            next_string_id: 0,

            used_registers: UseBitmap::empty(),
            tmp_registers: UseBitmap::empty(),
            tmp_symbol_registers: UseBitmap::empty(),
            tmp_symbol_id: HashMap::default(),
            function_stack_size: 0,
            reservations: Vec::new(),
            inflight_symbols: HashMap::new(),
            global_register: None,
            current_use_order: SymbolUseOrder::first(),
        }
    }

    fn reset_for_function(&mut self) {
        debug_assert!(self.arg_patch.is_empty());

        self.max_arg = 0;

        self.used_registers = UseBitmap::empty();
        self.tmp_registers = UseBitmap::empty();
        self.inflight_symbols.clear();
        self.global_register = None;
        self.current_use_order = SymbolUseOrder::first();
        self.function_stack_size = 0;
        self.reservations.clear();
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

    fn emit_move(&mut self, dst: Reg, src: Reg) -> Result<Option<InstrOffset>> {
        if dst == src {
            return Ok(None);
        }
        self.emit(Instruction::Move { dst, src }).map(Some)
    }

    fn next_instruction(&self) -> Result<InstrOffset> {
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
