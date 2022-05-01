use crate::{
    atom::Atoms,
    cell::CellOwner,
    gc::{self, Gc, Rebind, Trace, Tracer},
    instructions::GcByteCode,
    object::{ObjectFlags, ObjectKind, VmFunction},
    realm::exec::InstructionReader,
    rebind, root, GcObject, Object, Value,
};

mod builtin;
mod exec;
mod stack;
use self::stack::Stack;
pub use self::stack::{GcUpvalueObject, UpvalueObject};

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

    fn trace(&self, trace: Tracer) {
        trace.mark(self.function);
        self.this.trace(trace);
        self.new_target.trace(trace);
    }
}

pub struct Realm<'gc, 'cell> {
    builtin: builtin::Builtin<'gc, 'cell>,
    pub(crate) stack: Stack<'gc, 'cell>,
}

pub type GcRealm<'gc, 'cell> = Gc<'gc, 'cell, Realm<'gc, 'cell>>;

unsafe impl<'gc, 'cell> Trace for Realm<'gc, 'cell> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, trace: Tracer) {
        self.builtin.trace(trace);
    }
}

unsafe impl<'a, 'gc, 'cell> Rebind<'a> for Realm<'gc, 'cell> {
    type Output = Realm<'a, 'cell>;
}

impl<'gc, 'cell: 'gc> Realm<'gc, 'cell> {
    pub fn new<'rt>(
        owner: &mut CellOwner<'cell>,
        arena: &'gc gc::Arena<'rt, 'cell>,
        atoms: &Atoms,
    ) -> Self {
        let builtin = builtin::Builtin::new(owner, arena, atoms);
        let stack = Stack::new();
        Realm { builtin, stack }
    }
}

impl<'gc, 'cell: 'gc> GcRealm<'gc, 'cell> {
    pub fn push(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &gc::Arena<'_, 'cell>,
        v: &[Value<'_, 'cell>],
    ) {
        let b = self.borrow_mut(owner, arena);
        for &v in v {
            unsafe { b.stack.push(v) };
        }
    }

    /// # Safety
    ///
    /// The bytecode must be valid
    #[allow(unused_unsafe)]
    pub unsafe fn eval<'l>(
        self,
        arena: &'l mut gc::Arena<'_, 'cell>,
        owner: &mut CellOwner<'cell>,
        atoms: &Atoms,
        bc: GcByteCode<'_, 'cell>,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        let vm_function = VmFunction {
            bc,
            function: 0,
            upvalues: Box::new([]),
        };
        let function = Object::new(None, ObjectFlags::ORDINARY, ObjectKind::VmFn(vm_function));
        let function = arena.add(function);
        root!(arena, function);

        let frame_size = bc.borrow(owner).functions[0].size;
        let mut instr = InstructionReader::new_unsafe(owner, bc, 0);

        let ctx = ExecutionContext {
            function,
            this: Value::undefined(),
            new_target: Value::undefined(),
        };

        let guard = self.unsafe_borrow_mut(owner).stack.push_frame(frame_size);
        let res = self.run(arena, owner, atoms, &mut instr, &ctx);
        let res = rebind!(arena, res);
        self.unsafe_borrow_mut(owner).stack.pop_frame(arena, guard);
        res
    }
}

impl<'gc, 'cell> Gc<'gc, 'cell, Realm<'gc, 'cell>> {
    #[inline]
    pub fn global(&self, owner: &CellOwner<'cell>) -> GcObject<'gc, 'cell> {
        self.borrow(owner).builtin.global
    }
}
