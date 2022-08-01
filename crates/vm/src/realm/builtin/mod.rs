use dreck::{Bound, Owner, Root, Trace, Tracer};

use crate::object::{GcObject, Object, ObjectKind, ObjectFlags};



pub struct Builtin<'gc,'own>{
    pub global: GcObject<'gc,'own>,

    pub object_proto: GcObject<'gc,'own>,
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
    }
}

unsafe impl<'from, 'to, 'own> Bound<'to> for Builtin<'from, 'own> {
    type Rebound = Builtin<'to, 'own>;

}

pub struct BuiltinBuilder<'gc,'own>{
    owner: &'gc mut Owner<'own>,
    root: &'gc Root<'own>,
}

pub struct ObjectBuilder<'gc,'own>{
    root: &'gc Root<'own>,
    prototype: Option<GcObject<'gc,'own>>,
    flags: ObjectFlags,
    kind: ObjectKind<'gc,'own>,
}

impl<'gc,'own> ObjectBuilder<'gc,'own>{
    fn prototype(mut self, prototype: GcObject<'gc,'own>) -> Self{
        self.prototype = Some(prototype);
        self
    }

    fn flags(mut self, flags: ObjectFlags) -> Self{
        self.flags = flags;
        self
    }

    fn kind(mut self, kind: ObjectKind<'gc,'own>) -> Self{
        self.kind = kind;
        self
    }

    fn build(self) -> GcObject<'gc,'own>{
        Object::new_gc(self.root, self.prototype, self.flags, self.kind)
    }
}

impl<'gc,'own> BuiltinBuilder<'gc,'own>{
    pub fn object(&self) -> ObjectBuilder<'gc,'own>{
        ObjectBuilder{
            root: self.root,
            prototype: None,
            flags: ObjectFlags::ORDINARY,
            kind: ObjectKind::Ordinary
        }
    }

    pub fn build(self) -> Builtin<'gc,'own>{
        let op = self.object().build();
        let global = self.object().prototype(op).build();

        Builtin { global, object_proto: op }
    }
}

