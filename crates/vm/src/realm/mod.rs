use std::{alloc::Global, marker::PhantomData};

use ast::SymbolTable;
use common::interner::Interner;

use crate::{
    function::Function, gc::Trace, instructions::ByteCode, object::Object, value::BoundValue, Gc,
    GcArena, Value,
};

mod stack;
use stack::Stack;
mod ctx;
mod exec;
pub use ctx::{Arguments, RealmCtx};
mod reader;
mod runtime;
pub use reader::InstructionReader;
mod environment;

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
        let mut this = Realm {
            symbol_table,
            interner,
            gc,
            global,
            stack,
        };
        unsafe { runtime::init(this.context()) };
        this
    }

    pub unsafe fn eval(&mut self, bc: Gc<ByteCode>) -> Result<Value, ()> {
        self.stack.enter(bc.functions[0].registers);
        let reader = InstructionReader::from_bc(bc, 0);
        self.execute(reader)
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
        F: for<'a> Fn(RealmCtx<'a>, Arguments<'a>) -> BoundValue<'a> + 'static,
    {
        self.gc.allocate(Function::from_native(f))
    }

    pub(crate) unsafe fn context<'a>(&'_ mut self) -> RealmCtx<'a> {
        RealmCtx {
            realm: self,
            marker: PhantomData,
        }
    }

    pub(crate) unsafe fn arguments<'a>(&'_ mut self) -> Arguments<'a> {
        self.stack.arguments()
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
