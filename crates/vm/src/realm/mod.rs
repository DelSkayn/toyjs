use crate::{
    cell::CellOwner,
    gc::{self, Gc, Rebind, Trace, Tracer},
    instructions::GcByteCode,
    object::{ObjectKind, VmFunction},
    realm::exec::InstructionReader,
    GcObject, Object, Value,
};

use self::stack::Stack;

mod exec;
mod stack;

pub struct ExecutionContext<'gc, 'cell> {
    pub function: GcObject<'gc, 'cell>,
    pub this: Value<'gc, 'cell>,
    pub new_target: Value<'gc, 'cell>,
}

unsafe impl<'gc, 'cell> Trace for ExecutionContext<'gc, 'cell> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, trace: Tracer<'a>) {
        trace.mark(self.function);
        self.this.trace(trace);
        self.new_target.trace(trace);
    }
}

pub struct Realm<'gc, 'cell> {
    global: GcObject<'gc, 'cell>,
    stack: Stack<'gc, 'cell>,
}

unsafe impl<'gc, 'cell> Trace for Realm<'gc, 'cell> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, trace: Tracer<'a>) {
        trace.mark(self.global);
    }
}

unsafe impl<'a, 'gc, 'cell> Rebind<'a> for Realm<'gc, 'cell> {
    type Output = Realm<'a, 'cell>;
}

impl<'gc, 'cell: 'gc> Realm<'gc, 'cell> {
    pub fn new<'rt>(arena: &'gc gc::Arena<'rt, 'cell>) -> Self {
        let global = arena.add(Object::new(None, ObjectKind::Ordinary));
        let stack = Stack::new();
        Realm { global, stack }
    }

    /// # Safety
    ///
    /// The bytecode must be valid
    pub unsafe fn eval<'l>(
        &mut self,
        arena: &'l mut gc::Arena<'_, 'cell>,
        owner: &mut CellOwner<'cell>,
        bc: GcByteCode<'_, 'cell>,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        let vm_function = VmFunction { bc, function: 0 };
        let function = Object::new(None, ObjectKind::VmFn(vm_function));
        let function = arena.add(function);
        let __guard = arena._root(function);
        #[allow(unused_unsafe)]
        let function = unsafe { crate::gc::_rebind_to(&__guard, function) };

        let instr = InstructionReader::new_unsafe(owner, bc, 0);

        let ctx = ExecutionContext {
            function,
            this: Value::undefined(),
            new_target: Value::undefined(),
        };

        self.run(arena, owner, instr, ctx)
    }
}

impl<'gc, 'cell> Gc<'gc, 'cell, Realm<'gc, 'cell>> {
    #[inline]
    pub fn global(&self, owner: &CellOwner<'cell>) -> GcObject<'gc, 'cell> {
        self.borrow(owner).global
    }
}
