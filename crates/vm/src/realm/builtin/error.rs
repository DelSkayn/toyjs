use crate::{atom, object::ObjectFlags, realm::ExecutionContext, Gc, Object, Realm, Value};

use super::BuiltinAccessor;

pub struct Error;
impl BuiltinAccessor for Error {
    fn access(builtin: &super::Builtin) -> Option<Gc<Object>> {
        builtin.error_proto
    }
}

pub struct SyntaxError;
impl BuiltinAccessor for SyntaxError {
    fn access(builtin: &super::Builtin) -> Option<Gc<Object>> {
        builtin.syntax_error_proto
    }
}

pub struct TypeError;
impl BuiltinAccessor for TypeError {
    fn access(builtin: &super::Builtin) -> Option<Gc<Object>> {
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
        obj.index(atom::constant::prototype, realm)
    } else {
        Value::undefined()
    };
    let proto = proto
        .into_object()
        .unwrap_or_else(|| T::access(&realm.builtin).unwrap());

    let (message, options) = if realm.stack.frame_size() >= 1 {
        let message = realm.stack.read(0);
        let message = realm.to_string(message)?;
        let message = realm.vm().allocate::<String>(message.as_str().into());
        let options = if realm.stack.frame_size() >= 2 {
            Some(realm.stack.read(1))
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
    options: Option<Value>,
) -> Gc<Object> {
    let object = Object::alloc_error(realm, Some(prototype));
    if let Some(message) = message {
        object.index_set(atom::constant::message, message.into(), realm)
    }
    if let Some(options) = options.and_then(|x| x.into_object()) {
        let value = options.index(atom::constant::cause, realm);
        if !value.is_undefined() {
            object.index_set(atom::constant::cause, value, realm);
        }
    }
    object
}

pub unsafe fn to_string(realm: &Realm, exec: &mut ExecutionContext) -> Result<Value, Value> {
    let this = exec.this;
    let this = this
        .into_object()
        .ok_or_else(|| realm.create_type_error("this is not an object"))?;

    let name = this.index(atom::constant::name, realm);
    let name = realm.to_string(name)?;

    let message = this.index(atom::constant::message, realm);
    let message = realm.to_string(message)?;

    let res = realm
        .vm
        .allocate(format!("{}: {}", name.as_str(), message.as_str()));
    Ok(res.into())
}

pub unsafe fn init_native<T: BuiltinAccessor>(
    realm: &Realm,
    name: Gc<String>,
    construct_proto: Gc<Object>,
    proto_proto: Gc<Object>,
) -> (Gc<Object>, Gc<Object>) {
    let error_proto = Object::alloc(realm, Some(proto_proto), ObjectFlags::empty());
    let error_construct = Object::alloc_constructor(
        realm,
        Some(construct_proto),
        crate::object::FunctionKind::Static(construct_vm::<T>),
    );

    error_construct.index_set(atom::constant::prototype, error_proto.into(), realm);
    error_proto.index_set(atom::constant::constructor, error_construct.into(), realm);

    error_proto.index_set(
        atom::constant::message,
        realm.vm().allocate(String::new()).into(),
        realm,
    );
    error_proto.index_set(atom::constant::name, name.into(), realm);

    (error_construct, error_proto)
}
