use crate::{gc::Trace, object::FunctionKind, Gc, Object, Realm, Value};

use super::ExecutionContext;

pub struct Builtin<U: 'static> {
    // the %Object% value
    pub object_proto: Option<Gc<Object<U>>>,
    pub object_construct: Option<Gc<Object<U>>>,
    pub function_proto: Option<Gc<Object<U>>>,
    pub error_proto: Option<Gc<Object<U>>>,
}

fn object_construct<U: Trace>(
    realm: &mut Realm<U>,
    exec: &mut ExecutionContext<U>,
) -> Result<Value, Value> {
    unsafe {
        if exec.new_target.is_empty()
            || exec.new_target.is_undefined()
            || (exec.new_target.is_object()
                && exec.new_target.unsafe_cast_object().ptr_eq(exec.function))
        {
            if realm.stack.frame_size() == 0 {
                return Ok(realm.create_object().into());
            }
            let value = realm.stack.read(0);
            if value.is_undefined() || value.is_null() {
                return Ok(realm.create_object().into());
            }
            return Ok(realm.to_object(value).into());
        }

        let proto = if exec.new_target.is_object() {
            let key = realm.create_string("prototype");
            exec.new_target
                .unsafe_cast_object()
                .index(key.into(), realm)
        } else {
            Value::undefined()
        };
        let proto = if proto.is_object() {
            proto.unsafe_cast_object()
        } else {
            realm.builtin.error_proto.unwrap()
        };

        let object = Object::new_error(Some(proto));
        let object = realm.gc.allocate(object);
        Ok(object.into())
    }
}

fn function_proto<U: Trace>(_: &mut Realm<U>, _: &mut ExecutionContext<U>) -> Result<Value, Value> {
    Ok(Value::undefined())
}

fn error_construct<U: Trace>(
    realm: &mut Realm<U>,
    exec: &mut ExecutionContext<U>,
) -> Result<Value, Value> {
    unsafe {
        let new_target = if exec.new_target.is_empty() {
            exec.function.into()
        } else {
            exec.new_target
        };

        let proto = if new_target.is_object() {
            let key = realm.create_string("prototype");
            new_target.unsafe_cast_object().index(key.into(), realm)
        } else {
            Value::undefined()
        };
        let proto = if proto.is_object() {
            proto.unsafe_cast_object()
        } else {
            realm.builtin.error_proto.unwrap()
        };

        let object = Object::new_error(Some(proto));
        let object = realm.gc.allocate(object);

        if realm.stack.frame_size() >= 1 {
            let message = realm.stack.read(0);
            let message = realm.to_string(message);
            let message = realm.create_string(message);
            let key = realm.create_string("message");
            object.raw_index_set(key.into(), message.into(), realm);
        }
        if realm.stack.frame_size() >= 2 {
            let options = realm.stack.read(1);
            if options.is_object() {
                let key = realm.create_string("cause");
                let value = options.unsafe_cast_object().index(key.into(), realm);
                if !value.is_undefined() {
                    object.raw_index_set(key.into(), value, realm)
                }
            }
        }
        Ok(object.into())
    }
}

impl<U> Builtin<U> {
    pub const fn new() -> Self {
        Self {
            object_proto: None,
            object_construct: None,
            function_proto: None,
            error_proto: None,
        }
    }
}

unsafe impl<U> Trace for Builtin<U> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        self.object_proto.map(|x| ctx.mark(x));
        self.object_construct.map(|x| ctx.mark(x));
        self.function_proto.map(|x| ctx.mark(x));
        self.error_proto.map(|x| ctx.mark(x));
    }
}

impl<U> Default for Builtin<U> {
    fn default() -> Self {
        Builtin::new()
    }
}

impl<U: Trace> Realm<U> {
    pub unsafe fn init_builtin(&mut self) {
        let key_prototype = self.create_string("prototype").into();
        let key_constructor = self.create_string("constructor").into();
        let key_length = self.create_string("length").into();
        let key_empty = self.create_string("").into();

        let object_proto = self.create_object_proto(None);
        self.builtin.object_proto = Some(object_proto);

        let object_construct = self.create_constructor(object_construct);
        object_construct.raw_index_set(key_prototype, object_proto.into(), self);
        object_construct.raw_index_set(key_length, 1.into(), self);
        object_proto.raw_index_set(key_constructor, object_construct.into(), self);
        self.builtin.object_construct = Some(object_construct);

        let function_proto = Object::new_function(
            FunctionKind::Static(function_proto),
            self.builtin.object_proto,
        );
        let func_proto = self.gc.allocate(function_proto);
        self.builtin.function_proto = Some(func_proto);

        let error_proto = self.create_object();
        let error_construct = self.create_constructor(error_construct);

        error_construct.raw_index_set(key_prototype, error_proto.into(), self);
        error_proto.raw_index_set(key_constructor, error_construct.into(), self);

        let key = self.create_string("message").into();
        error_proto.raw_index_set(key, key_empty, self);
        let key = self.create_string("name").into();
        let value = self.create_string("Error").into();
        error_proto.raw_index_set(key, value, self);
        self.builtin.error_proto = Some(error_proto);

        let global = self.create_object();
        let key = self.create_string("Object").into();
        global.raw_index_set(key, object_construct.into(), self);
        let key = self.create_string("Error").into();
        global.raw_index_set(key, error_construct.into(), self);
        self.global = global;
    }
}
