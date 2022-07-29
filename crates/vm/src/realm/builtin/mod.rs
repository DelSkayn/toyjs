use common::atom::{self, Atoms};
use dreck::{Bound, Owner, Root, Trace, Tracer};

use crate::{
    object::{ObjectFlags, ObjectKind, PropertyFlags, StaticFn},
    GcObject, Object, Value,
};

use self::error::ErrorType;

use super::{ExecutionContext, GcRealm};

pub mod array;
mod boolean;
pub mod error;
mod number;
mod object;
mod string;

pub struct Builtin<'gc, 'own> {
    /// The global object.
    pub global: GcObject<'gc, 'own>,
    pub object_proto: GcObject<'gc, 'own>,
    pub function_proto: GcObject<'gc, 'own>,

    pub array_proto: GcObject<'gc, 'own>,

    pub string_proto: GcObject<'gc, 'own>,
    pub number_proto: GcObject<'gc, 'own>,
    pub boolean_proto: GcObject<'gc, 'own>,

    pub error_proto: GcObject<'gc, 'own>,
    pub syntax_error_proto: GcObject<'gc, 'own>,
    pub type_error_proto: GcObject<'gc, 'own>,
    pub runtime_error_proto: GcObject<'gc, 'own>,
}

unsafe impl<'gc, 'own> Trace<'own> for Builtin<'gc, 'own> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, trace: Tracer<'a,'own>) {
        trace.mark(self.global);
        trace.mark(self.object_proto);
        trace.mark(self.function_proto);
        trace.mark(self.array_proto);
        trace.mark(self.string_proto);
        trace.mark(self.number_proto);
        trace.mark(self.boolean_proto);
        trace.mark(self.error_proto);
        trace.mark(self.syntax_error_proto);
        trace.mark(self.type_error_proto);
        trace.mark(self.runtime_error_proto);
    }
}

unsafe impl<'a, 'gc, 'own> Bound<'a> for Builtin<'gc, 'own> {
    type Rebound = Builtin<'a, 'own>;
}

fn function_proto<'l, 'own>(
    _arena: &'l mut Root< 'own>,
    _owner: &mut Owner<'own>,
    _atoms: &Atoms,
    _realm: GcRealm<'_, 'own>,
    _ctx: &ExecutionContext<'_, 'own>,
) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
    Ok(Value::undefined())
}

fn new_func<'l, 'own>(
    arena: &'l Root< 'own>,
    owner: &mut Owner<'own>,
    atoms: &Atoms,
    prototype: GcObject<'_, 'own>,
    f: StaticFn,
    len: i32,
) -> GcObject<'l, 'own> {
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

impl<'gc, 'own> Builtin<'gc, 'own> {
    pub fn new(owner: &mut Owner<'own>, arena: &'gc Root< 'own>, atoms: &Atoms) -> Self {
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
        let string_proto = string::init(owner, arena, atoms, op, fp, global);
        let number_proto = number::init(owner, arena, atoms, op, fp, global);
        let boolean_proto = boolean::init(owner, arena, atoms, op, fp, global);

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
            string_proto,
            number_proto,
            boolean_proto,
            error_proto,
            syntax_error_proto,
            type_error_proto,
            runtime_error_proto,
        }
    }
}
