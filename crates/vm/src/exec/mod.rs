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
mod operator;
mod relation;

pub struct ExecutionContext<'l, 'gc, 'own> {
    pub root: &'l mut Root<'own>,
    pub owner: &'l mut Owner<'own>,

    pub realm: GcRealm<'gc, 'own>,
    pub stack: GcStack<'gc, 'own>,

    pub atoms: &'l mut Atoms<'gc, 'own>,

    /// The value this in the current context
    pub this: Value<'gc, 'own>,
    /// The current new_target
    pub new_target: Value<'gc, 'own>,
    /// The current function object
    pub function: GcObject<'gc, 'own>,
}

impl<'r, 'l: 'r, 'gc, 'own> ExecutionContext<'l, 'gc, 'own> {
    /// Create an exeuction context for entering into the vm.
    pub fn new(
        root: &'l mut Root<'own>,
        owner: &'l mut Owner<'own>,
        realm: GcRealm<'gc, 'own>,
        stack: GcStack<'gc, 'own>,
        atoms: &'l mut Atoms<'gc, 'own>,
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

    /// Run the current execution context
    pub fn eval(&'r mut self) -> Result<Value<'r, 'own>, Value<'r, 'own>> {
        match unsafe { self.function.borrow(self.owner).kind() } {
            ObjectKind::VmFn(x) => {
                let size = x.bc.borrow(self.owner).functions[x.function as usize].size as usize;
                debug_assert!(x.bc.borrow(self.owner).functions[x.function as usize]
                    .upvalues
                    .is_empty());
                let bc = x.bc;
                let function = x.function;
                Stack::push_frame(self.stack, self.owner, self.root, FrameType::Entry { size });
                let reader = InstructionReader::new(self.owner, bc, function);
                let res = unsafe { self.dispatch(reader) };
                Stack::pop_frame(self.stack, self.owner, self.root);
                res
            }
            ObjectKind::StaticFn(x) => rebind!(self.root, x(self)),
            _ => panic!("invalid function object kind"),
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

    pub fn arg(&self, idx: usize) -> Option<Value<'gc, 'own>> {
        todo!()
    }
}
