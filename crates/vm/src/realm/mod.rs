//! Implements the realm within wich scripts are executed.

use std::alloc::Global;

use ast::SymbolTable;
use common::interner::Interner;

use crate::{
    gc::Trace,
    instructions::ByteCode,
    object::{ConstructorFn, Object},
    Gc, GcArena, Value,
};

mod stack;
pub use stack::{Stack, UpvalueObject};
mod exec;
mod reader;
pub use reader::InstructionReader;

use self::{builtin::Builtin, environment::Environment, exec::ExecutionContext};
mod environment;

mod builtin;

pub struct Realm {
    pub symbol_table: SymbolTable<Global>,
    pub interner: Interner,
    pub gc: GcArena,
    pub global: Gc<Object>,
    pub stack: Stack,
    pub root: Gc<Environment>,
    pub builtin: Builtin,
}

impl Realm {
    pub fn new() -> Self {
        let symbol_table = SymbolTable::new();
        let interner = Interner::new();
        let gc = GcArena::new();
        let global = gc.allocate(Object::new(None));
        let stack = Stack::new();
        let mut res = Realm {
            builtin: Builtin::new(),
            symbol_table,
            interner,
            global,
            stack,
            root: gc.allocate(Environment::root()),
            gc,
        };
        unsafe {
            res.init_builtin();
        }
        res
    }

    pub unsafe fn eval(&mut self, bc: Gc<ByteCode>) -> Result<Value, Value> {
        self.stack.enter(bc.functions[0].registers);
        let instr = InstructionReader::from_bc(bc, 0);
        let context = ExecutionContext {
            function: self.construct_function_root(&instr),
            instr,
            this: self.global.into(),
            new_target: Value::null(),
        };
        self.execute(context)
    }

    pub unsafe fn global(&self) -> Gc<Object> {
        self.global
    }

    pub unsafe fn create_object(&self, prototype: Option<Gc<Object>>) -> Gc<Object> {
        self.gc.allocate(Object::new(prototype))
    }

    pub unsafe fn create_base_object(&self) -> Gc<Object> {
        self.create_object(self.builtin.object_proto)
    }

    pub unsafe fn create_string(&self, s: String) -> Gc<String> {
        self.gc.allocate(s)
    }

    pub unsafe fn create_shared_function<F>(&self, f: F) -> Gc<Object>
    where
        F: for<'a> Fn(&mut Realm) -> Result<Value, Value> + 'static,
    {
        self.gc.allocate(Object::from_shared(self, f))
    }

    pub unsafe fn create_constructor(&self, f: ConstructorFn) -> Gc<Object> {
        self.gc.allocate(Object::from_constructor(self, f))
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
