use std::alloc::Global;

use ast::SymbolTable;
use common::interner::Interner;

use crate::{gc::Trace, instructions::ByteCode, object::Object, Gc, GcArena, Value};

mod reader;
mod stack;
use stack::Stack;
mod exec;

pub struct Realm {
    pub symbol_table: SymbolTable<Global>,
    pub interner: Interner,
    pub gc: GcArena,
    pub global: Gc<Object>,
    pub stack: Stack,
}

impl Realm {
    pub fn new() -> Self {
        let symbol_table = SymbolTable::new();
        let interner = Interner::new();
        let gc = GcArena::new();
        let global = gc.allocate(Object::new());
        let stack = Stack::new();
        Realm {
            symbol_table,
            interner,
            gc,
            global,
            stack,
        }
    }

    pub fn eval(&mut self, bc: Gc<ByteCode>) -> Result<Value, ()> {
        let reader = reader::InstructionReader::from_bc(bc, 0);
        self.stack.enter(bc.functions[0].registers);
        unsafe { self.execute(reader) }
    }
}

unsafe impl Trace for Realm {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        ctx.mark(self.global);
        self.stack.trace(ctx);
    }
}
