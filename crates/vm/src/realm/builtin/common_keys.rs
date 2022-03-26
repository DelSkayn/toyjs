use crate::{gc::Trace, Gc, Realm};

pub struct CommonKeys {
    pub empty: Gc<String>,
    pub prototype: Gc<String>,
    pub constructor: Gc<String>,
    pub name: Gc<String>,
    pub message: Gc<String>,
    pub length: Gc<String>,
}

unsafe impl Trace for CommonKeys {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        ctx.mark(self.empty);
        ctx.mark(self.prototype);
        ctx.mark(self.constructor);
        ctx.mark(self.name);
        ctx.mark(self.message);
        ctx.mark(self.length);
    }
}

impl CommonKeys {
    pub unsafe fn new(realm: &mut Realm) -> Self {
        CommonKeys {
            empty: realm.vm().allocate::<String>("".into()),
            prototype: realm.vm().allocate::<String>("prototype".into()),
            constructor: realm.vm().allocate::<String>("constructor".into()),
            name: realm.vm().allocate::<String>("name".into()),
            message: realm.vm().allocate::<String>("message".into()),
            length: realm.vm().allocate::<String>("length".into()),
        }
    }
}
