use std::alloc::Global;

use ast::SymbolTable;
use common::interner::Interner;

use crate::{
    function::Function, gc::Trace, instructions::ByteCode, object::Object, Gc, GcArena, Value,
};

mod stack;
pub use stack::{Stack, UpvalueObject};
mod exec;
mod reader;
pub use reader::InstructionReader;

use self::environment::Environment;
mod environment;

pub struct Realm {
    pub symbol_table: SymbolTable<Global>,
    pub interner: Interner,
    pub gc: GcArena,
    pub global: Gc<Object>,
    pub stack: Stack,
    pub root: Gc<Environment>,
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
            global,
            stack,
            root: gc.allocate(Environment::root()),
            gc,
        }
    }

    pub unsafe fn eval(&mut self, bc: Gc<ByteCode>) -> Result<Value, Value> {
        self.stack.enter(bc.functions[0].registers);
        let reader = InstructionReader::from_bc(bc, 0);
        let func = self.construct_function_root(&reader);
        self.execute(reader, func)
    }

    pub unsafe fn global(&self) -> Gc<Object> {
        self.global
    }

    pub unsafe fn create_object(&self) -> Gc<Object> {
        self.gc.allocate(Object::new())
    }

    pub unsafe fn create_string(&self, s: String) -> Gc<String> {
        self.gc.allocate(s)
    }

    pub unsafe fn create_function<F>(&self, f: F) -> Gc<Function>
    where
        F: for<'a> Fn(&mut Realm) -> Value + 'static,
    {
        self.gc.allocate(Function::from_native(f))
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
