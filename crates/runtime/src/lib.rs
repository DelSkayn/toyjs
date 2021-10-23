#![allow(dead_code)]
#![feature(allocator_api)]

use ast::SymbolTable;

pub mod gc;
use common::interner::Interner;
use gc::Trace;
pub use gc::{Gc, GcArena};
pub mod instructions;
pub mod value;
use instructions::{Instruction, InstructionBuffer, InstructionReader};
pub use value::JSValue;
pub mod object;
use object::Object;
pub mod stack;
use stack::Stack;
mod function;

pub mod env;
pub mod exec;

use std::{alloc::Global, collections::VecDeque, fmt};

#[derive(Clone, Copy)]
pub struct ByteFunction {
    pub offset: usize,
    pub size: usize,
    pub registers: u8,
}

pub struct ByteCode {
    pub constants: Box<[JSValue]>,
    pub functions: Box<[ByteFunction]>,
    pub instructions: InstructionBuffer,
}

unsafe impl Trace for ByteCode {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: gc::Ctx) {
        self.constants.iter().for_each(|x| x.trace(ctx))
    }
}

impl fmt::Display for ByteCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "> CONSTANTS")?;
        for (idx, c) in self.constants.iter().enumerate() {
            writeln!(f, "{:>4}: {:?}", idx, c)?;
        }
        writeln!(f)?;
        writeln!(f, "> INSTRUCTIONS")?;
        for (idx, func) in self.functions.iter().enumerate() {
            writeln!(
                f,
                "= FUNC:{:<4} registers:{:<2} instructions:{}",
                idx, func.registers, func.size
            )?;

            let mut reader = InstructionReader::new(&self.instructions, func.offset, func.size);
            let mut idx = 0;

            while !reader.at_end() {
                write!(f, "{:>4}: ", idx)?;
                Instruction::format_byte_instruction(f, &mut reader)?;
                writeln!(f)?;
                idx += 1;
            }
        }
        Ok(())
    }
}

pub struct Task {
    pub bc: Gc<ByteCode>,
    pub function: usize,
}

pub struct Realm {
    pub symbol_table: SymbolTable<Global>,
    pub interner: Interner,
    pub gc: GcArena,
    pub global: Gc<Object>,
    pub tasks: VecDeque<Task>,
    pub stack: Stack,
}

impl Realm {
    pub fn new() -> Self {
        let gc = GcArena::new();
        let global = gc.allocate(Object::new());

        let mut res = Realm {
            symbol_table: SymbolTable::new(),
            interner: Interner::new(),
            global,
            tasks: VecDeque::new(),
            gc,
            stack: Stack::new(),
        };
        unsafe { env::initialize(&mut res) };
        res
    }

    pub fn schedule_task(&mut self, bc: Gc<ByteCode>, function: usize) {
        self.tasks.push_back(Task { bc, function })
    }

    pub fn execute_pending_tasks(&mut self) {
        while let Some(task) = self.tasks.pop_front() {
            self.execute_task(&task);
        }
    }

    pub fn execute_task(&mut self, task: &Task) -> JSValue {
        let function = &task.bc.functions[task.function];
        let mut reader =
            InstructionReader::new(&task.bc.instructions, function.offset, function.size);
        self.stack.enter_call(function.registers);
        unsafe { self.execute(&mut reader, task.bc) }
    }
}

unsafe impl Trace for Realm {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: gc::Ctx) {
        self.tasks.iter().for_each(|x| ctx.mark(x.bc));
        self.stack.trace(ctx);
        ctx.mark(self.global);
    }
}

impl Drop for Realm {
    fn drop(&mut self) {
        unsafe { self.gc.collect_all() };
    }
}
