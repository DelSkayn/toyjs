#![allow(dead_code)]
#![allow(unused_imports)]

use common::{interner::Interner, source::Source};
use compiler::Compiler;
use parser::{Parser, Result};
use runtime::{
    bytecode::Module,
    gc::{Ctx, Gc, GcArena, Trace},
};
pub use std::{
    cell::UnsafeCell,
    sync::{Arc, Mutex, Weak},
};

mod value;
pub use value::Value;
mod context;
pub use context::Context;
mod unsafe_mutex;
use unsafe_mutex::UnsafeMutex;

struct Root {
    contexts: UnsafeCell<Vec<Weak<Context>>>,
    use_stack: Vec<Gc<dyn Trace>>,
}

struct Inner {
    gc: GcArena,
    interner: Interner,
    root: Root,
}

unsafe impl Trace for Root {
    fn trace(&self, ctx: Ctx) {
        unsafe {
            (*self.contexts.get()).retain(|x| {
                if let Some(x) = x.upgrade() {
                    x.trace(ctx);
                    true
                } else {
                    false
                }
            });
            self.use_stack.iter().for_each(|x| {
                let gc: Gc<_> = *x;
                ctx.mark_dynamic(gc)
            });
        }
    }
}

#[derive(Clone)]
pub struct ToyJs(Arc<UnsafeMutex<Inner>>);

impl ToyJs {
    pub fn new() -> Self {
        ToyJs(Arc::new(UnsafeMutex::new(Inner {
            interner: Interner::new(),
            gc: GcArena::new(),
            root: Root {
                contexts: UnsafeCell::new(Vec::new()),
                use_stack: Vec::new(),
            },
        })))
    }
}
