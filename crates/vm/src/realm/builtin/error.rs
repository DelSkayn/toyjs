use crate::{object::ObjectFlags, realm::ExecutionContext, Gc, Object, Realm, Value};

use super::{common_keys::CommonKeys, BuiltinAccessor};

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
        let key = realm.vm().allocate::<String>("prototype".into());
        obj.index(key.into(), realm)?
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
        let key = realm.vm().allocate::<String>("message".into());
        object
            .raw_index_set(key.into(), message.into(), realm)
            .unwrap();
    }
    if let Some(options) = options {
        if options.is_object() {
            let key = realm.vm().allocate::<String>("cause".into());
            let value = options
                .unsafe_cast_object()
                .index(key.into(), realm)
                .unwrap();
            if !value.is_undefined() {
                object.raw_index_set(key.into(), value, realm).unwrap()
            }
        }
    }
    object
}

pub unsafe fn to_string(realm: &Realm, exec: &mut ExecutionContext) -> Result<Value, Value> {
    let this = exec.this;
    let this = this
        .into_object()
        .ok_or_else(|| realm.create_type_error("this is not an object"))?;

    let key = realm.vm().allocate::<String>("name".into());
    let name = this.index(key.into(), realm).unwrap();
    let name = realm.to_string(name)?;

    let key = realm.vm().allocate::<String>("message".into());
    let message = this.index(key.into(), realm).unwrap();
    let message = realm.to_string(message)?;

    let res = realm
        .vm
        .allocate(format!("{}: {}", name.as_str(), message.as_str()));
    Ok(res.into())
}

pub unsafe fn init_native<T: BuiltinAccessor>(
    realm: &Realm,
    keys: &CommonKeys,
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

    error_construct
        .raw_index_set(keys.prototype.into(), error_proto.into(), realm)
        .unwrap();
    error_proto
        .raw_index_set(keys.constructor.into(), error_construct.into(), realm)
        .unwrap();

    error_proto
        .raw_index_set(keys.message.into(), keys.empty.into(), realm)
        .unwrap();
    let key = realm.vm().allocate::<String>("name".into()).into();
    error_proto.raw_index_set(key, name.into(), realm).unwrap();

    (error_construct, error_proto)
}
