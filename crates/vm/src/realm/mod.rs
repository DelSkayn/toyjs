use dreck::{Bound, Gc, Root, Trace};

pub mod stack;
pub use stack::GcStack;

mod reader;
pub use reader::InstructionReader;

mod builtin;

use crate::object::{GcObject, Object, ObjectFlags};

use self::stack::Stack;

pub type GcRealm<'gc, 'own> = Gc<'gc, 'own, Realm<'gc, 'own>>;

pub struct Realm<'gc, 'own> {
    pub(crate) global: GcObject<'gc, 'own>,
    pub(crate) stack: GcStack<'gc, 'own>,
}

impl<'gc, 'own> Realm<'gc, 'own> {
    pub fn new(root: &'gc Root<'own>) -> Self {
        let stack = Stack::new();
        let stack = root.add(stack);
        let global = Object::new_gc(
            root,
            None,
            ObjectFlags::ORDINARY,
            crate::object::ObjectKind::Ordinary,
        );
        Realm { stack, global }
    }
}

unsafe impl<'gc, 'own> Trace<'own> for Realm<'gc, 'own> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, tracer: dreck::Tracer<'a, 'own>) {
        tracer.mark(self.stack)
    }
}

unsafe impl<'from, 'to, 'own> Bound<'to> for Realm<'from, 'own> {
    type Rebound = Realm<'to, 'own>;
}
