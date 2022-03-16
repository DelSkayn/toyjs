#![feature(allocator_api)]
#![allow(dead_code)]

use std::cell::RefCell;

mod ctx;
mod ffi;
use ctx::ContextInner;
pub use ctx::Ctx;
mod function;
pub use function::Function;
mod object;
pub use object::Object;
mod string;
pub use string::String;
mod value;
pub use value::Value;

mod runtime;

pub struct Context {
    inner: RefCell<ContextInner>,
}

impl Context {
    pub fn new() -> Self {
        let res = Context {
            inner: RefCell::new(ContextInner::new()),
        };
        res.with(runtime::init);
        res
    }

    pub fn with<'js, F: FnOnce(Ctx<'js>) -> R, R>(&self, f: F) -> R {
        let mut guard = self.inner.borrow_mut();
        unsafe {
            guard.push_frame();
            let ctx = Ctx::wrap(&mut *guard);
            let res = f(ctx);
            guard.pop_frame();
            res
        }
    }
}
