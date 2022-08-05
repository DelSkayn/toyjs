use crate::{
    atom::IntoAtom,
    object::{GcObject, Object, ObjectFlags, ObjectKind, Property, PropertyFlags},
    value::Value,
};

use super::BuiltinBuilder;

pub struct ObjectBuilder<'l, 'gc, 'own> {
    pub builder: &'l mut BuiltinBuilder<'gc, 'own>,
    pub prototype: Option<GcObject<'gc, 'own>>,
    pub flags: ObjectFlags,
    pub kind: ObjectKind<'gc, 'own>,
}

impl<'l, 'gc, 'own> ObjectBuilder<'l, 'gc, 'own> {
    pub fn prototype(mut self, prototype: GcObject<'gc, 'own>) -> Self {
        self.prototype = Some(prototype);
        self
    }

    pub fn flags(mut self, flags: ObjectFlags) -> Self {
        self.flags = flags;
        self
    }

    pub fn kind(mut self, kind: ObjectKind<'gc, 'own>) -> Self {
        self.kind = kind;
        self
    }

    pub fn build(self) -> GcObject<'gc, 'own> {
        Object::new_gc(self.builder.root, self.prototype, self.flags, self.kind)
    }
    pub fn props(self) -> PropBuilder<'l, 'gc, 'own> {
        PropBuilder {
            object: Object::new_gc(self.builder.root, self.prototype, self.flags, self.kind),
            builder: self.builder,
        }
    }
}

pub struct PropBuilder<'l, 'gc, 'own> {
    pub builder: &'l mut BuiltinBuilder<'gc, 'own>,
    pub object: GcObject<'gc, 'own>,
}

impl<'l, 'gc, 'own> PropBuilder<'l, 'gc, 'own> {
    pub fn set<K, V>(self, key: K, value: V) -> Self
    where
        K: IntoAtom<'own>,
        V: Into<Value<'gc, 'own>>,
    {
        let atom = key.into_atom(self.builder.root, self.builder.atoms);
        let property = Property::value(value.into(), PropertyFlags::BUILTIN, atom);

        let res = self
            .object
            .borrow_mut(self.builder.owner, self.builder.root)
            .properties
            .set(property);
        debug_assert!(res.is_none(), "set value on object which was already set");
        self
    }

    pub fn build(self) -> GcObject<'gc, 'own> {
        self.object
    }
}
