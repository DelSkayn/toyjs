use dreck::{Bound, Gc, Owner, Root, Trace, Tracer};

use crate::{
    atom::{self, Atoms},
    object::{GcObject, ObjectFlags, ObjectKind},
};

mod builder;
use builder::{ObjectBuilder, PropBuilder};

use self::error::ErrorType;

pub mod error;

pub struct Builtin<'gc, 'own> {
    pub global: GcObject<'gc, 'own>,

    pub object_proto: GcObject<'gc, 'own>,

    pub error_proto: GcObject<'gc, 'own>,
    pub syntax_error_proto: GcObject<'gc, 'own>,
    pub type_error_proto: GcObject<'gc, 'own>,
}

unsafe impl<'gc, 'own> Trace<'own> for Builtin<'gc, 'own> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, trace: Tracer<'a, 'own>) {
        trace.mark(self.global);
        trace.mark(self.object_proto);
    }
}

unsafe impl<'from, 'to, 'own> Bound<'to> for Builtin<'from, 'own> {
    type Rebound = Builtin<'to, 'own>;
}

pub struct BuiltinBuilder<'gc, 'own> {
    pub owner: &'gc mut Owner<'own>,
    pub root: &'gc Root<'own>,
    pub atoms: &'gc mut Atoms<'gc, 'own>,
}

impl<'gc, 'own> BuiltinBuilder<'gc, 'own> {
    pub fn object<'l>(&'l mut self) -> ObjectBuilder<'l, 'gc, 'own> {
        ObjectBuilder {
            builder: self,
            prototype: None,
            flags: ObjectFlags::ORDINARY,
            kind: ObjectKind::Ordinary,
        }
    }

    pub fn string<S: Into<String>>(&mut self, s: S) -> Gc<'gc, 'own, String> {
        self.root.add(s.into())
    }

    pub fn prop<'l>(&'l mut self, object: GcObject<'gc, 'own>) -> PropBuilder<'l, 'gc, 'own> {
        PropBuilder {
            object,
            builder: self,
        }
    }

    pub fn build(mut self) -> Builtin<'gc, 'own> {
        let op = self.object().build();
        let func_op = self.object().prototype(op).build();
        let global = self
            .object()
            .prototype(op)
            .props()
            .set(atom::constants::Object, op)
            .build();

        let name = self.string("Error");
        let (_, error_proto) = error::init::<{ ErrorType::Base as u8 }>(op, op, name, &mut self);

        let to_string = self
            .object()
            .prototype(func_op)
            .kind(ObjectKind::StaticFn(error::to_string))
            .build();

        self.prop(error_proto)
            .set(atom::constants::toString, to_string);

        let name = self.string("SyntaxError");
        let (_, syntax_error_proto) =
            error::init::<{ ErrorType::Syntax as u8 }>(op, op, name, &mut self);

        let name = self.string("TypeError");
        let (_, type_error_proto) =
            error::init::<{ ErrorType::Type as u8 }>(op, op, name, &mut self);

        Builtin {
            global,
            object_proto: op,
            error_proto,
            syntax_error_proto,
            type_error_proto,
        }
    }
}
