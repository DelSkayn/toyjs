use crate::{gc::Trace, object::FunctionKind, Gc, Object, Realm, Value};

pub struct Builtin<U: 'static> {
    // the %Object% value
    pub object_proto: Option<Gc<Object<U>>>,
    pub object_construct: Option<Gc<Object<U>>>,
    pub function_proto: Option<Gc<Object<U>>>,
}

fn object_construct<U: Trace>(
    realm: &mut Realm<U>,
    _: Value,
    target: Value,
) -> Result<Value, Value> {
    unsafe {
        if target.is_null() && realm.stack.frame_size() != 0 {
            Ok(realm.to_object(realm.stack.read(0)).into())
        } else {
            Ok(realm.create_object().into())
        }
    }
}

fn function_proto<U: Trace>(_: &mut Realm<U>) -> Result<Value, Value> {
    Ok(Value::undefined())
}

impl<U> Builtin<U> {
    pub const fn new() -> Self {
        Self {
            object_proto: None,
            object_construct: None,
            function_proto: None,
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
    }
}

impl<U> Default for Builtin<U> {
    fn default() -> Self {
        Builtin {
            object_proto: None,
            object_construct: None,
            function_proto: None,
        }
    }
}

impl<U: Trace> Realm<U> {
    pub unsafe fn init_builtin(&mut self) {
        self.builtin.object_proto = Some(self.create_object_proto(None));
        let function_proto = Object::new_function(
            FunctionKind::Static(function_proto),
            self.builtin.object_proto,
        );
        let func_proto = self.gc.allocate(function_proto);
        self.builtin.function_proto = Some(func_proto);
        self.builtin.object_construct = Some(self.create_constructor(object_construct));
    }
}
