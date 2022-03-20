//! Implements the realm within wich scripts are executed.

use std::any::Any;

use crate::{
    gc::Trace,
    instructions::ByteCode,
    object::{FunctionKind, Object, StaticFn, VmFunction, RECURSIVE_FUNC_PANIC},
    Gc, GcArena, Value,
};

mod stack;
pub use stack::{Stack, UpvalueObject};
mod exec;
mod reader;
pub use reader::InstructionReader;

use builtin::Builtin;
pub use exec::ExecutionContext;

mod builtin;

pub struct Realm {
    pub gc: GcArena,
    pub global: Gc<Object>,
    pub stack: Stack,
    pub builtin: Builtin,
    pub user_data: Box<dyn Any>,
}

impl Realm {
    pub fn new() -> Self {
        Self::new_with_user_data(())
    }
}

impl Realm {
    pub fn new_with_user_data<U: Any>(user_data: U) -> Self {
        let gc = GcArena::new();
        let global = gc.allocate(Object::new(None));
        let stack = Stack::new();
        let mut res = Realm {
            builtin: Builtin::new(),
            global,
            stack,
            gc,
            user_data: Box::new(user_data),
        };
        unsafe {
            res.init_builtin();
        }
        res
    }

    pub unsafe fn construct_script_function(&mut self, script: Gc<ByteCode>) -> Gc<Object> {
        debug_assert!(script.functions[0].upvalues.is_empty());
        self.gc.allocate(Object::from_vm(
            self,
            VmFunction {
                bc: script,
                function: 0,
                upvalues: Box::new([]),
            },
        ))
    }

    /// Call a function entering into the vm
    ///
    /// # panic
    /// Will panic if the object given is not a function
    pub unsafe fn enter_call(&mut self, function: Gc<Object>) -> Result<Value, Value> {
        let this = self.global().into();
        match function.function {
            Some(FunctionKind::Vm(ref x)) => {
                let instr = InstructionReader::from_bc(x.bc, x.function);
                self.stack.enter(instr.function(x.function).registers);
                self.execute(
                    instr,
                    ExecutionContext {
                        function,
                        this,
                        new_target: Value::empty(),
                    },
                )
            }
            Some(FunctionKind::Shared(ref x)) => {
                self.stack.enter(0);
                let mut ctx = ExecutionContext {
                    function,
                    this,
                    new_target: Value::empty(),
                };
                let res = x(self, &mut ctx);
                self.stack.pop(&self.gc);
                res
            }
            Some(FunctionKind::Static(x)) => {
                self.stack.enter(0);
                let mut ctx = ExecutionContext {
                    function,
                    this,
                    new_target: Value::empty(),
                };
                let res = x(self, &mut ctx);
                self.stack.pop(&self.gc);
                res
            }
            Some(FunctionKind::Mutable(ref x)) => {
                self.stack.enter(0);
                let mut ctx = ExecutionContext {
                    function,
                    this,
                    new_target: Value::empty(),
                };
                let res = x.try_borrow_mut().expect(RECURSIVE_FUNC_PANIC)(self, &mut ctx);
                self.stack.pop(&self.gc);
                res
            }
            None => panic!("enter_call called with object which was not a function"),
        }
    }

    pub unsafe fn eval(&mut self, bc: Gc<ByteCode>) -> Result<Value, Value> {
        let func = self.construct_script_function(bc);
        self.enter_call(func)
    }

    pub unsafe fn global(&self) -> Gc<Object> {
        self.global
    }

    pub unsafe fn create_object_proto(&self, prototype: Option<Gc<Object>>) -> Gc<Object> {
        self.gc.allocate(Object::new(prototype))
    }

    pub unsafe fn create_object(&self) -> Gc<Object> {
        self.create_object_proto(self.builtin.object_proto)
    }

    #[inline]
    pub unsafe fn create_string(&self, s: impl Into<String>) -> Gc<String> {
        self.gc.allocate(s.into())
    }

    pub unsafe fn create_shared_function<F>(&self, f: F) -> Gc<Object>
    where
        F: for<'a> Fn(&mut Realm, &mut ExecutionContext) -> Result<Value, Value> + 'static,
    {
        self.gc.allocate(Object::from_shared(self, f))
    }

    pub unsafe fn create_static_function(
        &self,
        f: fn(&mut Realm, &mut ExecutionContext) -> Result<Value, Value>,
    ) -> Gc<Object> {
        self.gc.allocate(Object::from_static(self, f))
    }

    pub unsafe fn create_constructor(&self, f: StaticFn) -> Gc<Object> {
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
        self.builtin.trace(ctx);
    }
}

impl Drop for Realm {
    fn drop(&mut self) {
        unsafe {
            self.gc.collect_all();
        }
    }
}
