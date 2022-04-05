use crate::{
    atom,
    gc::Trace,
    object::{FunctionKind, ObjectFlags},
    Gc, Object, Realm, Value, VmInner,
};

use super::ExecutionContext;

pub mod error;

pub trait BuiltinAccessor {
    fn access(builtin: &Builtin) -> Gc<Object>;
}

pub struct Builtin {
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
            return Ok(Object::alloc(
                realm.vm(),
                Some(realm.builtin.object_proto),
                ObjectFlags::empty(),
            )
            .into());
        }
        let value = realm.stack.read(0);
        if value.is_undefined() || value.is_null() {
            return Ok(Object::alloc(
                realm.vm(),
                Some(realm.builtin.object_proto),
                ObjectFlags::empty(),
            )
            .into());
        }
        return Ok(realm.to_object(value).into());
    }

    let proto = if let Some(object) = exec.new_target.into_object() {
        object.index(atom::constant::prototype, realm)
    } else {
        Value::undefined()
    };
    let proto = proto.into_object().unwrap_or(realm.builtin.error_proto);

    let object = Object::new_error(Some(proto));
    let object = realm.vm.borrow().allocate(object);
    Ok(object.into())
}

unsafe fn array_construct(realm: &Realm, exec: &mut ExecutionContext) -> Result<Value, Value> {
    let proto = if let Some(object) = exec.new_target.into_object() {
        object.index(atom::constant::prototype, realm)
    } else {
        Value::undefined()
    };
    let proto = proto.into_object().unwrap_or(realm.builtin.error_proto);

    if realm.stack.frame_size() == 0 {
        return Ok(Value::from(Object::alloc(
            realm.vm(),
            Some(proto),
            ObjectFlags::empty(),
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
    pub unsafe fn new(vm: &VmInner, global: Gc<Object>) -> Self {
        let object_proto = Object::alloc(vm, None, ObjectFlags::empty());

        let function_proto = Object::alloc_function(
            vm,
            Some(object_proto),
            ObjectFlags::empty(),
            FunctionKind::Static(function_proto),
        );

        object_proto.raw_index_set(
            atom::constant::toString,
            Object::alloc_function(
                vm,
                Some(function_proto),
                ObjectFlags::empty(),
                FunctionKind::Static(object_to_string),
            )
            .into(),
            vm,
        );
        let object_construct = Object::alloc_constructor(
            vm,
            Some(function_proto),
            FunctionKind::Static(object_construct),
        );
        object_construct.raw_index_set(atom::constant::prototype, object_proto.into(), vm);
        object_construct.raw_index_set(atom::constant::length, 1.into(), vm);
        object_proto.raw_index_set(atom::constant::constructor, object_construct.into(), vm);

        global.raw_index_set(atom::constant::Object, object_construct.into(), vm);

        let array_proto = Object::alloc(vm, Some(object_proto), ObjectFlags::empty());
        array_proto.raw_index_set(atom::constant::length, 0.into(), vm);

        let array_constructor = Object::alloc_constructor(
            vm,
            Some(function_proto),
            FunctionKind::Static(array_construct),
        );
        array_constructor.raw_index_set(atom::constant::prototype, array_proto.into(), vm);

        array_proto.raw_index_set(atom::constant::constructor, array_constructor.into(), vm);

        let name = vm.allocate::<String>("Error".into());
        let (error_construct, error_proto) =
            error::init_native::<error::Error>(vm, name, function_proto, object_proto);

        let func = Object::alloc_function(
            vm,
            Some(function_proto),
            ObjectFlags::empty(),
            FunctionKind::Static(error::to_string),
        );
        error_proto.raw_index_set(atom::constant::toString, func.into(), vm);
        global.raw_index_set(atom::constant::Error, error_construct.into(), vm);

        let name = vm.allocate::<String>("SyntaxError".into());
        let (error_construct, syntax_error_proto) =
            error::init_native::<error::TypeError>(vm, name, error_construct, error_proto);
        global.raw_index_set(atom::constant::SyntaxError, error_construct.into(), vm);

        let name = vm.allocate::<String>("TypeError".into());
        let (error_construct, type_error_proto) =
            error::init_native::<error::TypeError>(vm, name, error_construct, error_proto);
        global.raw_index_set(atom::constant::TypeError, error_construct.into(), vm);

        Builtin {
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
