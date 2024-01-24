use dreck::{rebind, Owner, Root};

use crate::{
    atom::Atoms,
    object::{GcObject, ObjectKind},
    realm::{
        self,
        stack::{FrameType, Stack},
        GcRealm, GcStack, InstructionReader,
    },
    value::Value,
};

mod convert;
mod dispatch;
mod function;
mod operator;
mod relation;

pub struct ExecutionContext<'gc, 'own> {
    pub root: &'gc mut Root<'own>,
    pub owner: &'gc mut Owner<'own>,

    pub realm: GcRealm<'gc, 'own>,
    pub stack: GcStack<'gc, 'own>,

    pub atoms: &'gc mut Atoms<'gc, 'own>,

    /// The value this in the current context
    pub this: Value<'gc, 'own>,
    /// The current new_target
    pub new_target: Value<'gc, 'own>,
    /// The current function object
    pub function: GcObject<'gc, 'own>,
}

impl<'r, 'gc: 'r, 'own> ExecutionContext<'gc, 'own> {
    /// Create an exeuction context for entering into the vm.
    pub fn new(
        root: &'gc mut Root<'own>,
        owner: &'gc mut Owner<'own>,
        realm: GcRealm<'gc, 'own>,
        stack: GcStack<'gc, 'own>,
        atoms: &'gc mut Atoms<'gc, 'own>,
        this: Value<'gc, 'own>,
        function: GcObject<'gc, 'own>,
    ) -> Self {
        ExecutionContext {
            root,
            owner,
            realm,
            stack,
            atoms,
            this,
            new_target: Value::undefined(),
            function,
        }
    }

    pub fn error(&'r mut self, msg: impl Into<String>) -> Value<'r, 'own> {
        let msg = self.root.add(msg.into());
        let proto = self.realm.borrow(self.owner).builtin.error_proto;
        realm::builtin::error::create(self.owner, self.root, proto, Some(msg), None).into()
    }

    pub fn syntax_error(&'r mut self, msg: impl Into<String>) -> Value<'r, 'own> {
        let msg = self.root.add(msg.into());
        let proto = self.realm.borrow(self.owner).builtin.syntax_error_proto;
        realm::builtin::error::create(self.owner, self.root, proto, Some(msg), None).into()
    }

    pub fn type_error(&'r mut self, msg: impl Into<String>) -> Value<'r, 'own> {
        let msg = self.root.add(msg.into());
        let proto = self.realm.borrow(self.owner).builtin.type_error_proto;
        realm::builtin::error::create(self.owner, self.root, proto, Some(msg), None).into()
    }

    pub fn arg(&self, idx: u32) -> Option<Value<'gc, 'own>> {
        unsafe { self.stack.borrow(self.owner).read_arg(idx) }
    }
}
