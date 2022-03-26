//! Implements the realm within wich scripts are executed.

use std::{any::Any, ptr::NonNull};

use crate::{
    gc::Trace,
    instructions::ByteCode,
    object::{FunctionKind, Object, ObjectFlags, VmFunction, RECURSIVE_FUNC_PANIC},
    vm::{Vm, VmInner},
    Gc, Value,
};

mod stack;
pub use stack::{Stack, UpvalueObject};
mod exec;
mod reader;
pub use reader::InstructionReader;

use builtin::Builtin;
pub use exec::ExecutionContext;

mod builtin;

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct VmBox {
    ptr: NonNull<VmInner>,
}

impl VmBox {
    unsafe fn new(vm: *const VmInner) -> Self {
        VmBox {
            ptr: NonNull::new(vm as *mut _).unwrap(),
        }
    }

    unsafe fn borrow(&self) -> &VmInner {
        &(*(self.ptr.as_ptr() as *const VmInner))
    }

    pub unsafe fn allocate<T: Trace + 'static>(&self, v: T) -> Gc<T> {
        (*self.ptr.as_ptr()).allocate(v)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct RealmBox(NonNull<Realm>);

impl RealmBox {
    pub fn new(realm: Realm) -> Self {
        let ptr = Box::into_raw(Box::new(realm));
        RealmBox(NonNull::new(ptr).unwrap())
    }

    pub unsafe fn free(self) {
        (*self.0.as_ptr()).vm.borrow().remove_realm(self);
        Box::from_raw(self.as_ptr());
    }

    pub fn as_ptr(self) -> *mut Realm {
        self.0.as_ptr()
    }
}

unsafe impl Trace for RealmBox {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        unsafe { (*self.0.as_ptr()).trace(ctx) }
    }
}

pub struct Realm {
    pub(crate) vm: VmBox,
    pub global: Gc<Object>,
    pub stack: Stack,
    pub builtin: Builtin,
    pub user_data: *mut dyn Any,
}

impl Realm {
    pub fn new(vm: &Vm) -> RealmBox {
        Self::new_with_user_data(vm, ())
    }

    pub fn new_with_user_data<U: Any>(vm: &Vm, user_data: U) -> RealmBox {
        unsafe {
            let vm = vm.0.lock();
            let vm = VmBox::new(&*vm);

            let global = vm
                .borrow()
                .allocate(Object::new(None, ObjectFlags::empty()));
            let stack = Stack::new();
            let mut res = Realm {
                vm,
                builtin: Builtin::new(),
                global,
                stack,
                user_data: Box::into_raw(Box::new(user_data)),
            };
            res.init_builtin();
            let res = RealmBox::new(res);
            vm.borrow().add_realm(res);
            res
        }
    }

    #[inline]
    pub unsafe fn vm(&self) -> &VmInner {
        self.vm.borrow()
    }

    pub unsafe fn construct_script_function(&self, script: Gc<ByteCode>) -> Gc<Object> {
        debug_assert!(script.functions[0].upvalues.is_empty());
        Object::alloc_function(
            self,
            self.builtin.function_proto,
            ObjectFlags::empty(),
            FunctionKind::Vm(VmFunction {
                bc: script,
                function: 0,
                upvalues: Box::new([]),
            }),
        )
    }

    /// Call a function entering into the vm
    ///
    /// # panic
    /// Will panic if the object given is not a function
    pub unsafe fn enter_call(&self, function: Gc<Object>) -> Result<Value, Value> {
        let this = self.global.into();
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
                self.stack.pop(&self.vm.borrow().gc());
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
                self.stack.pop(&self.vm.borrow().gc());
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
                self.stack.pop(&self.vm.borrow().gc());
                res
            }
            None => panic!("enter_call called with object which was not a function"),
        }
    }

    pub unsafe fn eval(&self, bc: Gc<ByteCode>) -> Result<Value, Value> {
        let func = self.construct_script_function(bc);
        self.enter_call(func)
    }

    pub unsafe fn user_enter_frame(&self, registers: u8) {
        self.stack.enter(registers)
    }

    pub unsafe fn user_pop_frame(&self) {
        self.stack.pop(self.vm.borrow().gc());
    }

    pub unsafe fn user_push_temp(&self, tmp: Value) {
        self.stack.push_temp(tmp)
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
        unsafe { Box::from_raw(self.user_data) };
    }
}
