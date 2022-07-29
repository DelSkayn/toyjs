use common::atom::{self, Atom, Atoms};

use dreck::{self, Bound, Gc, Owner, Root, Trace, Tracer, rebind, root};

use crate::{
    instructions::GcByteCode,
    object::{ObjectFlags, ObjectKind, VmFunction},
    realm::exec::InstructionReader,
    GcObject, Object, Value,
};

mod builtin;
mod exec;
mod stack;
pub use self::stack::{GcUpvalueObject, Stack, UpvalueObject};

pub struct ExecutionContext<'gc, 'own> {
    pub function: GcObject<'gc, 'own>,
    pub this: Value<'gc, 'own>,
    pub new_target: Value<'gc, 'own>,
}

unsafe impl<'gc, 'own> Trace<'own> for ExecutionContext<'gc, 'own> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, trace: Tracer) {
        trace.mark(self.function);
        self.this.trace(trace);
        self.new_target.trace(trace);
    }
}

pub struct Realm<'gc, 'own> {
    pub builtin: builtin::Builtin<'gc, 'own>,
    pub(crate) stack: Stack<'gc, 'own>,
}

pub type GcRealm<'gc, 'own> = Gc<'gc, 'own, Realm<'gc, 'own>>;

unsafe impl<'gc, 'own> Trace<'own> for Realm<'gc, 'own> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, trace: Tracer<'a,'own>) {
        self.builtin.trace(trace);
        self.stack.trace(trace);
    }
}

unsafe impl<'a, 'gc, 'own> Bound<'a> for Realm<'gc, 'own> {
    type Rebound = Realm<'a, 'own>;
}

impl<'gc, 'own: 'gc> Realm<'gc, 'own> {
    pub fn new(
        owner: &mut Owner<'own>,
        arena: &'gc Root<'own>,
        atoms: &Atoms,
    ) -> Self {
        let builtin = builtin::Builtin::new(owner, arena, atoms);
        let stack = Stack::new();
        Realm { builtin, stack }
    }

    pub fn atomize_primitive(
        owner: &Owner<'own>,
        atoms: &Atoms,
        value: Value<'gc, 'own>,
    ) -> Atom {
        if let Some(x) = value.into_atom() {
            return x;
        }
        if let Some(x) = value.into_bool() {
            if x {
                return atom::constant::r#true;
            } else {
                return atom::constant::r#false;
            }
        }
        if let Some(x) = value.into_int() {
            return Atoms::try_atomize_int(x)
                .unwrap_or_else(|| atoms.atomize_string(&x.to_string()));
        }

        if let Some(x) = value.into_float() {
            return atoms.atomize_string(&x.to_string());
        }

        atoms.atomize_string(value.into_string().unwrap().borrow(owner))
    }
}

impl<'gc, 'own> Realm<'gc, 'own> {
    pub fn argc(this: GcRealm<'gc,'own>, owner: &Owner<'own>) -> u32 {
        unsafe { this.borrow(owner).stack.frame_size() as u32 }
    }

    pub fn arg(this: GcRealm<'gc,'own>, owner: &Owner<'own>, idx: u32) -> Option<Value<'gc, 'own>> {
        unsafe { dreck::rebind(this.borrow(owner).stack.read_arg(idx)) }
    }

    pub fn push(
        this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &Root<'own>,
        v: &[Value<'_, 'own>],
    ) {
        let b = this.borrow_mut(owner, arena);
        for &v in v {
            unsafe { b.stack.push(v) };
        }
    }

    pub fn call<'l>(
        this: GcRealm<'gc,'own>,
        arena: &'l mut Root<'own>,
        owner: &mut Owner<'own>,
        atoms: &Atoms,
        function: GcObject<'_, 'own>,
    ) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
        let ctx = ExecutionContext {
            function,
            this: Value::undefined(),
            new_target: Value::undefined(),
        };

        unsafe { this.vm_call(owner, arena, atoms, ctx) }
    }

    /// # Safety
    ///
    /// The bytecode must be valid
    #[allow(unused_unsafe)]
    pub unsafe fn eval<'l>(
        this: GcRealm<'gc,'own>,
        arena: &'l mut Root<'own>,
        owner: &mut Owner<'own>,
        atoms: &Atoms,
        bc: GcByteCode<'_, 'own>,
    ) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
        let vm_function = VmFunction {
            bc,
            function: 0,
            upvalues: Box::new([]),
        };
        let function = Object::new(None, ObjectFlags::ORDINARY, ObjectKind::VmFn(vm_function));
        let function = arena.add(function);
        root!(arena, function);

        let frame_size = bc.borrow(owner).functions[0].registers as u32;
        let mut instr = InstructionReader::new_unsafe(owner, bc, 0);

        let ctx = ExecutionContext {
            function,
            this: Value::undefined(),
            new_target: Value::undefined(),
        };

        let guard = rebind_try!(
            arena,
            this.unsafe_borrow_mut(owner)
                .stack
                .push_frame(frame_size)
                .ok_or_else(|| {
                    this.create_runtime_error(owner, arena, atoms, Stack::DEPTH_EXCEEDED_MSG, None)
                })
        );
        let res = this.run(arena, owner, atoms, &mut instr, &ctx);
        let res = rebind!(arena, res);
        this.unsafe_borrow_mut(owner).stack.pop_frame(arena, guard);
        res
    }
}

impl<'gc, 'own> Realm<'gc, 'own> {
    #[inline]
    pub fn global(this: GcRealm<'gc,'own>, owner: &Owner<'own>) -> GcObject<'gc, 'own> {
        this.borrow(owner).builtin.global
    }
}
