//! Implements the realm within wich scripts are executed.

use std::alloc::Global;

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

pub struct Realm<U: 'static = ()> {
    pub gc: GcArena,
    pub global: Gc<Object<U>>,
    pub stack: Stack<U>,
    pub root: Gc<Environment>,
    pub builtin: Builtin<U>,
    pub user_data: U,
}

impl Realm {
    pub fn new() -> Self {
        Self::new_with_user_data(())
    }
}

impl<U: Trace> Realm<U> {
    pub fn new_with_user_data(user_data: U) -> Self {
        let gc = GcArena::new();
        let global = gc.allocate(Object::new(None));
        let stack = Stack::new();
        let mut res = Realm {
            builtin: Builtin::new(),
            global,
            stack,
            root: gc.allocate(Environment::root()),
            gc,
            user_data,
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

    pub unsafe fn global(&self) -> Gc<Object<U>> {
        self.global
    }

    pub unsafe fn create_object_proto(&self, prototype: Option<Gc<Object<U>>>) -> Gc<Object<U>> {
        self.gc.allocate(Object::new(prototype))
    }

    pub unsafe fn create_object(&self) -> Gc<Object<U>> {
        self.create_object_proto(self.builtin.object_proto)
    }

    pub unsafe fn create_string(&self, s: String) -> Gc<String> {
        self.gc.allocate(s)
    }

    pub unsafe fn create_shared_function<F>(&self, f: F) -> Gc<Object<U>>
    where
        F: for<'a> Fn(&mut Realm<U>) -> Result<Value, Value> + 'static,
    {
        self.gc.allocate(Object::from_shared(self, f))
    }

    pub unsafe fn create_static_function(
        &self,
        f: fn(&mut Realm<U>) -> Result<Value, Value>,
    ) -> Gc<Object<U>> {
        self.gc.allocate(Object::from_static(self, f))
    }

    pub unsafe fn create_constructor(&self, f: ConstructorFn<U>) -> Gc<Object<U>> {
        self.gc.allocate(Object::from_constructor(self, f))
    }
}

unsafe impl<U: Trace> Trace for Realm<U> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        ctx.mark(self.global);
        self.stack.trace(ctx);
        if U::needs_trace() {
            self.user_data.trace(ctx);
        }
    }
}
