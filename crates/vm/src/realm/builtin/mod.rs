use crate::{
    atom,
    gc::Trace,
    object::{ObjectFlags, ObjectKind, Property, PropertyFlags, StaticFn},
    Gc, Object, Realm, Value, VmInner,
};

use super::ExecutionContext;

pub mod error;
pub mod object;

pub trait BuiltinAccessor {
    fn access(builtin: &Builtin) -> Gc<Object>;
}

pub struct Builtin {
    pub global: Gc<Object>,
    // the %Object% value
    pub object_proto: Gc<Object>,
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
        ctx.mark(self.function_proto);
        ctx.mark(self.error_proto);
        ctx.mark(self.syntax_error_proto);
        ctx.mark(self.type_error_proto);
    }
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

fn new_func(vm: &VmInner, proto: Gc<Object>, f: StaticFn) -> Gc<Object> {
    Object::new_gc(
        vm,
        Some(proto),
        ObjectFlags::ORDINARY,
        ObjectKind::StaticFn(f),
    )
}

impl Builtin {
    pub unsafe fn new(vm: &VmInner) -> Self {
        // Short names because there used often
        // object prototype
        let op = Object::new_gc(vm, None, ObjectFlags::ORDINARY, ObjectKind::Ordinary);
        // function prototype
        let fp = Object::new_gc(
            vm,
            Some(op),
            ObjectFlags::ORDINARY,
            ObjectKind::StaticFn(function_proto),
        );

        op.raw_index_set(
            vm,
            atom::constant::toString,
            Object::new_gc(
                vm,
                Some(fp),
                ObjectFlags::ORDINARY,
                ObjectKind::StaticFn(object_to_string),
            )
            .into(),
        );
        let global = Object::new_gc(vm, Some(op), ObjectFlags::ORDINARY, ObjectKind::Ordinary);
        global.raw_index_set_prop(
            vm,
            atom::constant::globalThis,
            Property::new_value(
                global.into(),
                PropertyFlags::WRITABLE | PropertyFlags::CONFIGURABLE,
            ),
        );
        object::init(vm, op, fp, global);

        let array_proto = Object::new_gc(vm, Some(op), ObjectFlags::empty(), ObjectKind::Ordinary);
        array_proto.raw_index_set(vm, atom::constant::length, 0.into());

        let array_constructor = Object::new_gc(
            vm,
            Some(fp),
            ObjectFlags::ORDINARY | ObjectFlags::CONSTRUCTOR,
            ObjectKind::StaticFn(array_construct),
        );
        array_constructor.raw_index_set(vm, atom::constant::prototype, array_proto.into());

        array_proto.raw_index_set(vm, atom::constant::constructor, array_constructor.into());

        let name = vm.allocate::<String>("Error".into());
        let (error_construct, error_proto) = error::init_native::<error::Error>(vm, name, fp, op);

        let func = Object::new_gc(
            vm,
            Some(fp),
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
            object_proto: op,
            function_proto: fp,
            array_proto,
            error_proto,
            syntax_error_proto,
            type_error_proto,
        }
    }
}
