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
            empty: realm.create_string(""),
            prototype: realm.create_string("prototype"),
            constructor: realm.create_string("constructor"),
            name: realm.create_string("name"),
            message: realm.create_string("message"),
            length: realm.create_string("length"),
        }
    }
}
