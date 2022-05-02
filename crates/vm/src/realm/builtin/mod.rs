use crate::{
    atom::{self, Atoms},
    cell::CellOwner,
    gc::{Arena, Rebind, Trace},
    object::{ObjectFlags, ObjectKind, PropertyFlag, StaticFn},
    GcObject, Object, Value,
};

use super::{ExecutionContext, GcRealm};

mod object;

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
    _arena: &'l mut Arena<'_, 'cell>,
    _owner: &mut CellOwner<'cell>,
    _atoms: &Atoms,
    _realm: GcRealm<'_, 'cell>,
    _ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    Ok(Value::undefined())
}

fn new_func<'l, 'cell>(
    arena: &'l Arena<'_, 'cell>,
    owner: &mut CellOwner<'cell>,
    atoms: &Atoms,
    prototype: GcObject<'_, 'cell>,
    f: StaticFn,
    len: i32,
) -> GcObject<'l, 'cell> {
    let object = Object::new_gc(
        arena,
        Some(prototype),
        ObjectFlags::ORDINARY,
        ObjectKind::StaticFn(f),
    );
    object.raw_index_set_flags(
        owner,
        arena,
        atoms,
        atom::constant::length,
        len,
        PropertyFlag::CONFIGURABLE,
    );
    object
}

impl<'gc, 'cell> Builtin<'gc, 'cell> {
    pub fn new(owner: &mut CellOwner<'cell>, arena: &'gc Arena<'_, 'cell>, atoms: &Atoms) -> Self {
        let op = arena.add(Object::new(
            None,
            ObjectFlags::ORDINARY,
            ObjectKind::Ordinary,
        ));
        let global = arena.add(Object::new(
            Some(op),
            ObjectFlags::ORDINARY,
            ObjectKind::Ordinary,
        ));
        global.raw_index_set_flags(
            owner,
            arena,
            atoms,
            atom::constant::globalThis,
            global,
            PropertyFlag::WRITABLE | PropertyFlag::CONFIGURABLE,
        );
        let fp = arena.add(Object::new(
            Some(op),
            ObjectFlags::ORDINARY,
            ObjectKind::StaticFn(function_proto),
        ));

        object::init(owner, arena, atoms, op, fp, global);

        Builtin {
            global,
            object_proto: op,
            function_proto: fp,
        }
    }
}
