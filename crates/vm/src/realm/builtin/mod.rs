use crate::{
    atom,
    gc::Trace,
    object::{ObjectFlags, ObjectKind},
    Gc, Object, Realm, Value, VmInner,
};

use super::ExecutionContext;

pub mod error;

pub trait BuiltinAccessor {
    fn access(builtin: &Builtin) -> Gc<Object>;
}

pub struct Builtin {
    pub global: Gc<Object>,
    // the %Object% value
    pub object_proto: Gc<Object>,
    pub object_construct: Gc<Object>,
    pub function_proto: Gc<Object>,
    pub array_proto: Gc<Object>,
    pub error_proto: Gc<Object>,
    pub syntax_error_proto: Gc<Object>,
    pub type_error_proto: Gc<Object>,
}

unsafe impl Trace for Builtin {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        ctx.mark(self.global);
        ctx.mark(self.object_proto);
        ctx.mark(self.object_construct);
        ctx.mark(self.function_proto);
        ctx.mark(self.error_proto);
        ctx.mark(self.syntax_error_proto);
        ctx.mark(self.type_error_proto);
    }
}

unsafe fn object_construct(realm: &Realm, exec: &mut ExecutionContext) -> Result<Value, Value> {
    if exec.new_target.is_undefined()
        || exec
            .new_target
            .into_object()
            .map_or(false, |x| x.ptr_eq(exec.function))
    {
        if realm.stack.frame_size() == 0 {
            return Ok(Object::new_gc(
                realm.vm(),
                Some(realm.builtin.object_proto),
                ObjectFlags::empty(),
                ObjectKind::Ordinary,
            )
            .into());
        }
        let value = realm.stack.read(0);
        if value.is_undefined() || value.is_null() {
            return Ok(Object::new_gc(
                realm.vm(),
                Some(realm.builtin.object_proto),
                ObjectFlags::empty(),
                ObjectKind::Ordinary,
            )
            .into());
        }
        return Ok(realm.to_object(value).into());
    }

    let proto = if let Some(object) = exec.new_target.into_object() {
        object.index(realm, atom::constant::prototype)?
    } else {
        Value::undefined()
    };
    let proto = proto.into_object().unwrap_or(realm.builtin.error_proto);

    let object = Object::new_gc(
        realm.vm(),
        Some(proto),
        ObjectFlags::ORDINARY,
        ObjectKind::Ordinary,
    );
    Ok(object.into())
}

unsafe fn array_construct(realm: &Realm, exec: &mut ExecutionContext) -> Result<Value, Value> {
    let proto = if let Some(object) = exec.new_target.into_object() {
        object.index(realm, atom::constant::prototype)?
    } else {
        Value::undefined()
    };
    let proto = proto.into_object().unwrap_or(realm.builtin.array_proto);

    if realm.stack.frame_size() == 0 {
        return Ok(Value::from(Object::new_gc(
            realm.vm(),
            Some(proto),
            ObjectFlags::empty(),
            ObjectKind::Array,
        )));
    }

    todo!()
}

fn function_proto(_: &Realm, _: &mut ExecutionContext) -> Result<Value, Value> {
    Ok(Value::undefined())
}

unsafe fn object_to_string(realm: &Realm, _: &mut ExecutionContext) -> Result<Value, Value> {
    Ok(realm.vm().allocate("[object Object]".to_string()).into())
}

impl Builtin {
    pub unsafe fn new(vm: &VmInner) -> Self {
        let object_proto = Object::new_gc(vm, None, ObjectFlags::ORDINARY, ObjectKind::Ordinary);

        let function_proto = Object::new_gc(
            vm,
            Some(object_proto),
            ObjectFlags::ORDINARY,
            ObjectKind::StaticFn(function_proto),
        );

        object_proto.raw_index_set(
            vm,
            atom::constant::toString,
            Object::new_gc(
                vm,
                Some(function_proto),
                ObjectFlags::ORDINARY,
                ObjectKind::StaticFn(object_to_string),
            )
            .into(),
        );
        let object_construct = Object::new_gc(
            vm,
            Some(function_proto),
            ObjectFlags::ORDINARY | ObjectFlags::CONSTRUCTOR,
            ObjectKind::StaticFn(object_construct),
        );
        object_construct.raw_index_set(vm, atom::constant::prototype, object_proto.into());
        object_construct.raw_index_set(vm, atom::constant::length, 1.into());
        object_proto.raw_index_set(vm, atom::constant::constructor, object_construct.into());

        let global = Object::new_gc(
            vm,
            Some(object_proto),
            ObjectFlags::ORDINARY,
            ObjectKind::Ordinary,
        );

        global.raw_index_set(vm, atom::constant::globalThis, global.into());
        global.raw_index_set(vm, atom::constant::Object, object_construct.into());

        let array_proto = Object::new_gc(
            vm,
            Some(object_proto),
            ObjectFlags::empty(),
            ObjectKind::Ordinary,
        );
        array_proto.raw_index_set(vm, atom::constant::length, 0.into());

        let array_constructor = Object::new_gc(
            vm,
            Some(function_proto),
            ObjectFlags::ORDINARY | ObjectFlags::CONSTRUCTOR,
            ObjectKind::StaticFn(array_construct),
        );
        array_constructor.raw_index_set(vm, atom::constant::prototype, array_proto.into());

        array_proto.raw_index_set(vm, atom::constant::constructor, array_constructor.into());

        let name = vm.allocate::<String>("Error".into());
        let (error_construct, error_proto) =
            error::init_native::<error::Error>(vm, name, function_proto, object_proto);

        let func = Object::new_gc(
            vm,
            Some(function_proto),
            ObjectFlags::ORDINARY,
            ObjectKind::StaticFn(error::to_string),
        );
        error_proto.raw_index_set(vm, atom::constant::toString, func.into());
        global.raw_index_set(vm, atom::constant::Error, error_construct.into());

        let name = vm.allocate::<String>("SyntaxError".into());
        let (error_construct, syntax_error_proto) =
            error::init_native::<error::TypeError>(vm, name, error_construct, error_proto);
        global.raw_index_set(vm, atom::constant::SyntaxError, error_construct.into());

        let name = vm.allocate::<String>("TypeError".into());
        let (error_construct, type_error_proto) =
            error::init_native::<error::TypeError>(vm, name, error_construct, error_proto);
        global.raw_index_set(vm, atom::constant::TypeError, error_construct.into());

        Builtin {
            global,
            object_proto,
            object_construct,
            function_proto,
            array_proto,
            error_proto,
            syntax_error_proto,
            type_error_proto,
        }
    }
}
