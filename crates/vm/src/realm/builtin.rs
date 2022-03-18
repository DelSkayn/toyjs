use crate::{gc::Trace, object::FunctionKind, Gc, Object, Realm, Value};

pub struct Builtin {
    // the %Object% value
    pub object_proto: Option<Gc<Object>>,
    pub object_construct: Option<Gc<Object>>,
    pub function_proto: Option<Gc<Object>>,
}

fn object_construct(realm: &mut Realm, _: Value, target: Value) -> Result<Value, Value> {
    unsafe {
        if target.is_null() && realm.stack.frame_size() != 0 {
            Ok(realm.to_object(realm.stack.read(0)).into())
        } else {
            Ok(realm.create_base_object().into())
        }
    }
}

fn function_proto(_: &mut Realm) -> Result<Value, Value> {
    Ok(Value::undefined())
}

impl Builtin {
    pub const fn new() -> Self {
        Self {
            object_proto: None,
            object_construct: None,
            function_proto: None,
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
        self.object_proto.map(|x| ctx.mark(x));
        self.object_construct.map(|x| ctx.mark(x));
        self.function_proto.map(|x| ctx.mark(x));
    }
}

impl Default for Builtin {
    fn default() -> Self {
        Builtin {
            object_proto: None,
            object_construct: None,
            function_proto: None,
        }
    }
}

impl Realm {
    pub unsafe fn init_builtin(&mut self) {
        self.builtin.object_proto = Some(self.create_object(None));
        let function_proto = Object::new_function(
            FunctionKind::Static(function_proto),
            self.builtin.object_proto,
        );
        let func_proto = self.gc.allocate(function_proto);
        self.builtin.function_proto = Some(func_proto);
        self.builtin.object_construct = Some(self.create_constructor(object_construct));
    }
}
