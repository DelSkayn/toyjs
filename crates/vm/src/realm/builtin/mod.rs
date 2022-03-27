use crate::{
    gc::Trace,
    object::{FunctionKind, ObjectFlags},
    Gc, Object, Realm, Value,
};

use super::ExecutionContext;

mod common_keys;
use common_keys::CommonKeys;

pub mod error;

pub trait BuiltinAccessor {
    fn access(builtin: &Builtin) -> Option<Gc<Object>>;
}

pub struct Builtin {
    pub keys: Option<CommonKeys>,
    // the %Object% value
    pub object_proto: Option<Gc<Object>>,
    pub object_construct: Option<Gc<Object>>,
    pub function_proto: Option<Gc<Object>>,
    pub error_proto: Option<Gc<Object>>,
    pub syntax_error_proto: Option<Gc<Object>>,
    pub type_error_proto: Option<Gc<Object>>,
}

unsafe fn object_construct(realm: &Realm, exec: &mut ExecutionContext) -> Result<Value, Value> {
    if exec.new_target.is_empty()
        || exec.new_target.is_undefined()
        || (exec.new_target.is_object()
            && exec.new_target.unsafe_cast_object().ptr_eq(exec.function))
    {
        if realm.stack.frame_size() == 0 {
            return Ok(
                Object::alloc(realm, realm.builtin.object_proto, ObjectFlags::empty()).into(),
            );
        }
        let value = realm.stack.read(0);
        if value.is_undefined() || value.is_null() {
            return Ok(
                Object::alloc(realm, realm.builtin.object_proto, ObjectFlags::empty()).into(),
            );
        }
        return Ok(realm.to_object(value).into());
    }

    let proto = if exec.new_target.is_object() {
        let key = realm.vm().allocate::<String>("prototype".into());
        exec.new_target
            .unsafe_cast_object()
            .index(key.into(), realm)
            .unwrap()
    } else {
        Value::undefined()
    };
    let proto = if proto.is_object() {
        proto.unsafe_cast_object()
    } else {
        realm.builtin.error_proto.unwrap()
    };

    let object = Object::new_error(Some(proto));
    let object = realm.vm.borrow().allocate(object);
    Ok(object.into())
}

fn function_proto(_: &Realm, _: &mut ExecutionContext) -> Result<Value, Value> {
    Ok(Value::undefined())
}

unsafe fn object_to_string(realm: &Realm, _: &mut ExecutionContext) -> Result<Value, Value> {
    Ok(realm.vm().allocate("[object Object]".to_string()).into())
}

impl Builtin {
    pub const fn new() -> Self {
        Self {
            keys: None,
            object_proto: None,
            object_construct: None,
            function_proto: None,
            error_proto: None,
            syntax_error_proto: None,
            type_error_proto: None,
        }
    }
}

unsafe impl Trace for Builtin {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        if let Some(keys) = self.keys.as_ref() {
            keys.trace(ctx);
        }
        if let Some(x) = self.object_proto {
            ctx.mark(x);
        }
        if let Some(x) = self.object_construct {
            ctx.mark(x);
        }
        if let Some(x) = self.function_proto {
            ctx.mark(x);
        }
        if let Some(x) = self.error_proto {
            ctx.mark(x);
        }
        if let Some(x) = self.syntax_error_proto {
            ctx.mark(x);
        }
        if let Some(x) = self.type_error_proto {
            ctx.mark(x);
        }
    }
}

impl Default for Builtin {
    fn default() -> Self {
        Builtin::new()
    }
}

impl Realm {
    pub unsafe fn init_builtin(&mut self) {
        let keys = CommonKeys::new(self);

        let object_proto = Object::alloc(self, None, ObjectFlags::empty());
        let key_to_string = self.vm().allocate::<String>("toString".into());
        self.builtin.object_proto = Some(object_proto);

        let func_proto = Object::alloc_function(
            self,
            Some(object_proto),
            ObjectFlags::empty(),
            FunctionKind::Static(function_proto),
        );

        self.builtin.function_proto = Some(func_proto);

        object_proto
            .index_set(
                key_to_string.into(),
                Object::alloc_function(
                    self,
                    Some(func_proto),
                    ObjectFlags::empty(),
                    FunctionKind::Static(object_to_string),
                )
                .into(),
                self,
            )
            .unwrap();

        let object_construct = Object::alloc_constructor(
            self,
            Some(func_proto),
            FunctionKind::Static(object_construct),
        );
        object_construct
            .raw_index_set(keys.prototype.into(), object_proto.into(), self)
            .unwrap();
        object_construct
            .raw_index_set(keys.length.into(), 1.into(), self)
            .unwrap();
        object_proto
            .raw_index_set(keys.constructor.into(), object_construct.into(), self)
            .unwrap();
        self.builtin.object_construct = Some(object_construct);

        let global = Object::alloc(self, Some(object_proto), ObjectFlags::empty());
        let key = self.vm().allocate::<String>("Object".into()).into();
        global
            .raw_index_set(key, object_construct.into(), self)
            .unwrap();
        self.global = global;

        let name = self.vm().allocate::<String>("Error".into());
        let (error_construct, error_proto) =
            error::init_native::<error::Error>(self, &keys, name, func_proto, object_proto);

        let key = self.vm().allocate::<String>("toString".into());
        let func = Object::alloc_function(
            self,
            Some(func_proto),
            ObjectFlags::empty(),
            FunctionKind::Static(error::to_string),
        );
        error_proto
            .raw_index_set(key.into(), func.into(), self)
            .unwrap();
        self.builtin.error_proto = Some(error_proto);
        global
            .index_set(name.into(), error_construct.into(), self)
            .unwrap();

        let name = self.vm().allocate::<String>("SyntaxError".into());
        let (error_construct, error_proto) =
            error::init_native::<error::TypeError>(self, &keys, name, error_construct, error_proto);
        self.builtin.type_error_proto = Some(error_proto);
        global
            .index_set(name.into(), error_construct.into(), self)
            .unwrap();

        let name = self.vm().allocate::<String>("TypeError".into());
        let (error_construct, error_proto) =
            error::init_native::<error::TypeError>(self, &keys, name, error_construct, error_proto);
        self.builtin.type_error_proto = Some(error_proto);
        global
            .index_set(name.into(), error_construct.into(), self)
            .unwrap();

        self.builtin.keys = Some(keys);
    }
}
