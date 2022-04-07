use crate::{
    atom,
    object::{ObjectFlags, ObjectKind},
    realm::ExecutionContext,
    Gc, Object, Realm, Value, VmInner,
};

use super::BuiltinAccessor;

pub struct Error;
impl BuiltinAccessor for Error {
    fn access(builtin: &super::Builtin) -> Gc<Object> {
        builtin.error_proto
    }
}

pub struct SyntaxError;
impl BuiltinAccessor for SyntaxError {
    fn access(builtin: &super::Builtin) -> Gc<Object> {
        builtin.syntax_error_proto
    }
}

pub struct TypeError;
impl BuiltinAccessor for TypeError {
    fn access(builtin: &super::Builtin) -> Gc<Object> {
        builtin.type_error_proto
    }
}

pub unsafe fn construct_vm<T: BuiltinAccessor>(
    realm: &Realm,
    exec: &mut ExecutionContext,
) -> Result<Value, Value> {
    let new_target = if exec.new_target.is_undefined() {
        exec.function.into()
    } else {
        exec.new_target
    };

    let proto = if let Some(obj) = new_target.into_object() {
        obj.index(realm, atom::constant::prototype)?
    } else {
        Value::undefined()
    };
    let proto = proto
        .into_object()
        .unwrap_or_else(|| T::access(&realm.builtin));

    let (message, options) = if realm.stack.frame_size() >= 1 {
        let message = realm.stack.read(0);
        let message = realm.to_string(message)?;
        let message = realm.vm().allocate::<String>(message.as_str().into());
        let options = if realm.stack.frame_size() >= 2 {
            if let Some(options) = realm.stack.read(1).into_object() {
                let value = options.index(realm, atom::constant::cause)?;
                if !value.is_undefined() {
                    Some(value)
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };
        (Some(message), options)
    } else {
        (None, None)
    };
    Ok(construct(realm, proto, message, options).into())
}

pub unsafe fn construct(
    realm: &Realm,
    prototype: Gc<Object>,
    message: Option<Gc<String>>,
    cause: Option<Value>,
) -> Gc<Object> {
    let object = Object::new_gc(
        realm.vm(),
        Some(prototype),
        ObjectFlags::ORDINARY,
        ObjectKind::Error,
    );
    if let Some(message) = message {
        object.raw_index_set(realm.vm(), atom::constant::message, message.into())
    }
    if let Some(cause) = cause {
        object.raw_index_set(realm.vm(), atom::constant::cause, cause);
    }
    object
}

pub unsafe fn to_string(realm: &Realm, exec: &mut ExecutionContext) -> Result<Value, Value> {
    let this = exec.this;
    let this = this
        .into_object()
        .ok_or_else(|| realm.create_type_error("this is not an object"))?;

    let name = this.index(realm, atom::constant::name)?;
    let name = realm.to_string(name)?;

    let message = this.index(realm, atom::constant::message)?;
    let message = realm.to_string(message)?;

    let res = realm
        .vm
        .allocate(format!("{}: {}", name.as_str(), message.as_str()));
    Ok(res.into())
}

pub unsafe fn init_native<T: BuiltinAccessor>(
    vm: &VmInner,
    name: Gc<String>,
    construct_proto: Gc<Object>,
    proto_proto: Gc<Object>,
) -> (Gc<Object>, Gc<Object>) {
    let error_proto = Object::new_gc(
        vm,
        Some(proto_proto),
        ObjectFlags::empty(),
        ObjectKind::Ordinary,
    );
    let error_construct = Object::new_gc(
        vm,
        Some(construct_proto),
        ObjectFlags::ORDINARY | ObjectFlags::CONSTRUCTOR,
        ObjectKind::StaticFn(construct_vm::<T>),
    );

    error_construct.raw_index_set(vm, atom::constant::prototype, error_proto.into());
    error_proto.raw_index_set(vm, atom::constant::constructor, error_construct.into());

    error_proto.raw_index_set(
        vm,
        atom::constant::message,
        vm.allocate(String::new()).into(),
    );
    error_proto.raw_index_set(vm, atom::constant::name, name.into());

    (error_construct, error_proto)
}
