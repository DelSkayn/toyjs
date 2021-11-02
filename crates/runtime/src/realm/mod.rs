use ast::SymbolTable;

use crate::{
    gc::{self, Trace},
    instructions::{ByteCode, InstructionReader},
    object::Object,
    stack::Stack,
    Gc, GcArena, JSValue,
};
use common::interner::Interner;

use std::{alloc::Global, collections::VecDeque};

pub mod exec;

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

        Realm {
            symbol_table: SymbolTable::new(),
            interner: Interner::new(),
            global,
            tasks: VecDeque::new(),
            gc,
            stack: Stack::new(),
        }
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
