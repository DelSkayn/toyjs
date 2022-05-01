use crate::{
    atom::Atoms,
    cell::CellOwner,
    gc::{Arena, Rebind, Trace},
    object::ObjectKind,
    GcObject, Object, Value,
};

use super::{ExecutionContext, GcRealm};

pub struct Builtin<'gc, 'cell> {
    /// The global object.
    pub global: GcObject<'gc, 'cell>,
    pub object_proto: GcObject<'gc, 'cell>,
    pub function_proto: GcObject<'gc, 'cell>,
}

unsafe impl<'gc, 'cell> Trace for Builtin<'gc, 'cell> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, trace: crate::gc::Tracer) {
        trace.mark(self.global);
    }
}

unsafe impl<'a, 'gc, 'cell> Rebind<'a> for Builtin<'gc, 'cell> {
    type Output = Builtin<'a, 'cell>;
}

fn function_proto<'l, 'cell>(
    _arena: &mut Arena<'_, 'cell>,
    _owner: &'l mut CellOwner<'cell>,
    _realm: GcRealm<'_, 'cell>,
    _ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    Ok(Value::undefined())
}

impl<'gc, 'cell> Builtin<'gc, 'cell> {
    pub fn new(_atoms: &Atoms, arena: &'gc Arena<'_, 'cell>) -> Self {
        let op = arena.add(Object::new(None, ObjectKind::Ordinary));
        let global = arena.add(Object::new(Some(op), ObjectKind::Ordinary));
        let fp = arena.add(Object::new(Some(op), ObjectKind::StaticFn(function_proto)));

        Builtin {
            global,
            object_proto: op,
            function_proto: fp,
        }
    }
}
