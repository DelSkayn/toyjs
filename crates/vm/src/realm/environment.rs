use crate::{gc::Trace, Gc, Value};

pub enum UpValue {
    Closed(Value),
    Open(u8),
    UpValue(u8),
}

unsafe impl Trace for UpValue {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        match *self {
            UpValue::Closed(x) => x.trace(ctx),
            _ => {}
        }
    }
}

pub struct Environment {
    parent: Option<Gc<Environment>>,
    upvalues: Vec<UpValue>,
}

unsafe impl Trace for Environment {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        if let Some(x) = self.parent {
            ctx.mark(x)
        }
        self.upvalues.iter().for_each(|x| x.trace(ctx));
    }
}

impl Environment {
    pub fn new(parent: Gc<Environment>) -> Self {
        Environment {
            parent: Some(parent),
            upvalues: Vec::new(),
        }
    }

    pub fn root() -> Self {
        Environment {
            parent: None,
            upvalues: Vec::new(),
        }
    }
}
