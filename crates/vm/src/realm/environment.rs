use crate::{gc::Trace, Gc, Value};

pub struct Environment {
    parent: Option<Gc<Environment>>,
    upvalues: Vec<Value>,
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
    pub fn new(parent: Option<Gc<Environment>>) -> Self {
        Environment {
            parent,
            upvalues: Vec::new(),
        }
    }
}
