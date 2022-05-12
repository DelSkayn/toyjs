use common::atom::{self, Atoms};

use crate::{
    cell::CellOwner,
    gc::{Arena, Rebind, Trace},
    object::{ObjectFlags, ObjectKind, PropertyFlags, StaticFn},
    GcObject, Object, Value,
};

use self::error::ErrorType;

use super::{ExecutionContext, GcRealm};

pub mod array;
pub mod error;
mod object;

pub struct Builtin<'gc, 'cell> {
    /// The global object.
    pub global: GcObject<'gc, 'cell>,
    pub object_proto: GcObject<'gc, 'cell>,
    pub function_proto: GcObject<'gc, 'cell>,

    pub array_proto: GcObject<'gc, 'cell>,

    pub error_proto: GcObject<'gc, 'cell>,
    pub syntax_error_proto: GcObject<'gc, 'cell>,
    pub type_error_proto: GcObject<'gc, 'cell>,
    pub runtime_error_proto: GcObject<'gc, 'cell>,
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
        trace.mark(self.object_proto);
        trace.mark(self.function_proto);
        trace.mark(self.array_proto);
        trace.mark(self.error_proto);
        trace.mark(self.syntax_error_proto);
        trace.mark(self.type_error_proto);
        trace.mark(self.runtime_error_proto);
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
        PropertyFlags::CONFIGURABLE,
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
            PropertyFlags::WRITABLE | PropertyFlags::CONFIGURABLE,
        );
        let fp = arena.add(Object::new(
            Some(op),
            ObjectFlags::ORDINARY,
            ObjectKind::StaticFn(function_proto),
        ));

        object::init(owner, arena, atoms, op, fp, global);
        let array_proto = array::init(owner, arena, atoms, op, fp, global);

        let name = arena.add("Error".to_string());
        let error_to_string = new_func(arena, owner, atoms, fp, error::to_string, 0);
        let (error_construct, error_proto) =
            error::init::<{ ErrorType::Base as u8 }>(owner, arena, atoms, name, fp, op);
        error_proto.raw_index_set_flags(
            owner,
            arena,
            atoms,
            atom::constant::toString,
            error_to_string,
            PropertyFlags::BUILTIN,
        );

        let name = arena.add("SyntaxError".to_string());
        let (c, syntax_error_proto) = error::init::<{ ErrorType::Syntax as u8 }>(
            owner,
            arena,
            atoms,
            name,
            error_construct,
            error_proto,
        );
        global.raw_index_set_flags(
            owner,
            arena,
            atoms,
            atom::constant::SyntaxError,
            c,
            PropertyFlags::BUILTIN,
        );

        let name = arena.add("TypeError".to_string());
        let (c, type_error_proto) = error::init::<{ ErrorType::Type as u8 }>(
            owner,
            arena,
            atoms,
            name,
            error_construct,
            error_proto,
        );
        global.raw_index_set_flags(
            owner,
            arena,
            atoms,
            atom::constant::TypeError,
            c,
            PropertyFlags::BUILTIN,
        );

        let name = arena.add("RuntimeError".to_string());
        let (_, runtime_error_proto) = error::init::<{ ErrorType::Type as u8 }>(
            owner,
            arena,
            atoms,
            name,
            error_construct,
            error_proto,
        );

        Builtin {
            global,
            object_proto: op,
            function_proto: fp,
            array_proto,
            error_proto,
            syntax_error_proto,
            type_error_proto,
            runtime_error_proto,
        }
    }
}
