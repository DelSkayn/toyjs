use crate::{
    atom,
    gc::Trace,
    object::{ObjectFlags, ObjectKind, PropertyFlags, StaticFn},
    Gc, Object, Realm, Value, VmInner,
};

use super::ExecutionContext;

pub mod array;
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
        ctx.mark(self.array_proto);
        ctx.mark(self.error_proto);
        ctx.mark(self.syntax_error_proto);
        ctx.mark(self.type_error_proto);
    }
}

fn function_proto(_: &Realm, _: &mut ExecutionContext) -> Result<Value, Value> {
    Ok(Value::undefined())
}

fn new_func(vm: &VmInner, proto: Gc<Object>, f: StaticFn, len: i32) -> Gc<Object> {
    let object = Object::new_gc(
        vm,
        Some(proto),
        ObjectFlags::ORDINARY,
        ObjectKind::StaticFn(f),
    );
    object.raw_index_set_flags(vm, atom::constant::length, len, PropertyFlags::CONFIGURABLE);
    object
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

        let global = Object::new_gc(vm, Some(op), ObjectFlags::ORDINARY, ObjectKind::Ordinary);
        global.raw_index_set_flags(
            vm,
            atom::constant::globalThis,
            global,
            PropertyFlags::WRITABLE | PropertyFlags::CONFIGURABLE,
        );
        object::init(vm, op, fp, global);
        let array_proto = array::init(vm, op, fp, global);

        let name = vm.allocate::<String>("Error".into());
        let (error_construct, error_proto) = error::init_native::<error::Error>(vm, name, fp, op);

        let func = Object::new_gc(
            vm,
            Some(fp),
            ObjectFlags::ORDINARY,
            ObjectKind::StaticFn(error::to_string),
        );
        error_proto.raw_index_set_flags(vm, atom::constant::toString, func, PropertyFlags::BUILTIN);
        global.raw_index_set_flags(
            vm,
            atom::constant::Error,
            error_construct,
            PropertyFlags::BUILTIN,
        );

        let name = vm.allocate::<String>("SyntaxError".into());
        let (error_construct, syntax_error_proto) =
            error::init_native::<error::TypeError>(vm, name, error_construct, error_proto);
        global.raw_index_set_flags(
            vm,
            atom::constant::SyntaxError,
            error_construct,
            PropertyFlags::BUILTIN,
        );

        let name = vm.allocate::<String>("TypeError".into());
        let (error_construct, type_error_proto) =
            error::init_native::<error::TypeError>(vm, name, error_construct, error_proto);
        global.raw_index_set_flags(
            vm,
            atom::constant::TypeError,
            error_construct,
            PropertyFlags::BUILTIN,
        );

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
