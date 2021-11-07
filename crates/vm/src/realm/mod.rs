use ast::SymbolTable;

use crate::{
    function::Function,
    gc::{self, Trace},
    instructions::{ByteCode, InstructionReader},
    object::Object,
    stack::Stack,
    value::BoundValue,
    Gc, GcArena,
};
use common::interner::Interner;

use std::{alloc::Global, collections::VecDeque};

pub mod exec;
pub mod runtime;

enum Task {
    Timeout(Gc<Function>),
}

unsafe impl Trace for Task {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: gc::Ctx) {
        match *self {
            Task::Timeout(x) => ctx.mark(x),
        }
    }
}

pub struct Realm {
    pub symbol_table: SymbolTable<Global>,
    pub interner: Interner,
    pub gc: GcArena,
    pub global: Gc<Object>,
    tasks: VecDeque<Task>,
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
        unsafe { runtime::initialize(&mut res) };
        res
    }

    pub fn eval<'a>(&'a mut self, bc: ByteCode) -> BoundValue<'a> {
        unsafe {
            let bc = self.gc.allocate(bc);
            let func = bc.functions[0];
            self.stack.enter_call(func.registers);
            let mut reader = InstructionReader::new(&bc.instructions, func.offset, func.size);
            let res = self.execute(&mut reader, bc);
            self.stack.exit_call();
            BoundValue::bind(res)
        }
    }

    pub fn push_task(&mut self, task: Gc<Function>) {
        self.tasks.push_back(Task::Timeout(task))
    }

    pub fn execute_pending_task(&mut self) -> bool {
        if let Some(task) = self.tasks.pop_front() {
            match task {
                Task::Timeout(x) => {
                    unsafe { x.call(self) };
                }
            }
            true
        } else {
            false
        }
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
        self.tasks.iter().for_each(|x| x.trace(ctx));
        self.stack.trace(ctx);
        ctx.mark(self.global);
    }
}

impl Drop for Realm {
    fn drop(&mut self) {
        unsafe { self.gc.collect_all() };
    }
}
