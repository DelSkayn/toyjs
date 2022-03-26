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

pub fn construct<T: BuiltinAccessor>(
    realm: &mut Realm,
    exec: &mut ExecutionContext,
) -> Result<Value, Value> {
    unsafe {
        let new_target = if exec.new_target.is_empty() {
            exec.function.into()
        } else {
            exec.new_target
        };

        let proto = if new_target.is_object() {
            let key = realm.vm().allocate::<String>("prototype".into());
            new_target.unsafe_cast_object().index(key.into(), realm)
        } else {
            Value::undefined()
        };
        let proto = if proto.is_object() {
            proto.unsafe_cast_object()
        } else {
            T::access(&realm.builtin).unwrap()
        };

        let object = Object::new_error(Some(proto));
        let object = realm.vm.borrow().allocate(object);

        if realm.stack.frame_size() >= 1 {
            let message = realm.stack.read(0);
            let message = realm.to_string(message);
            let message = realm.vm().allocate::<String>(message.as_str().into());
            let key = realm.vm().allocate::<String>("message".into());
            object.raw_index_set(key.into(), message.into(), realm);
        }
        if realm.stack.frame_size() >= 2 {
            let options = realm.stack.read(1);
            if options.is_object() {
                let key = realm.vm().allocate::<String>("cause".into());
                let value = options.unsafe_cast_object().index(key.into(), realm);
                if !value.is_undefined() {
                    object.raw_index_set(key.into(), value, realm)
                }
            }
        }
        Ok(object.into())
    }
}

pub unsafe fn init_native<T: BuiltinAccessor>(
    realm: &mut Realm,
    keys: &CommonKeys,
    name: Gc<String>,
    construct_proto: Gc<Object>,
    proto_proto: Gc<Object>,
) -> (Gc<Object>, Gc<Object>) {
    let error_proto = Object::alloc(realm, Some(proto_proto), ObjectFlags::empty());
    let error_construct = Object::alloc_constructor(
        realm,
        Some(construct_proto),
        crate::object::FunctionKind::Static(construct::<T>),
    );

    error_construct.raw_index_set(keys.prototype.into(), error_proto.into(), realm);
    error_proto.raw_index_set(keys.constructor.into(), error_construct.into(), realm);

    error_proto.raw_index_set(keys.message.into(), keys.empty.into(), realm);
    let key = realm.vm().allocate::<String>("name".into()).into();
    error_proto.raw_index_set(key, name.into(), realm);

    (error_construct, error_proto)
}
