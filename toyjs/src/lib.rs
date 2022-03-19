#![feature(allocator_api)]
#![allow(dead_code)]

use std::cell::RefCell;

mod ctx;
mod ffi;
pub use ctx::Ctx;
use ctx::UserData;
mod function;
pub use function::Function;
mod object;
pub use object::Object;
mod string;
pub use string::String;
mod value;
pub use value::Value;

pub mod convert;

mod runtime;

pub struct Context {
    inner: RefCell<vm::Realm>,
}

impl Context {
    pub fn new() -> Self {
        let res = Context {
            inner: RefCell::new(vm::Realm::new_with_user_data(UserData::new())),
        };
        res.with(runtime::init);
        res
    }

    pub fn collect_all(&self) {
        let inner = self.inner.borrow_mut();
        unsafe {
            inner.gc.collect_full(&*inner);
        }
    }

    pub fn with<F, R>(&self, f: F) -> R
    where
        F: for<'js> FnOnce(Ctx<'js>) -> R,
    {
        let mut guard = self.inner.borrow_mut();
        unsafe {
            let mut ctx = Ctx::wrap(&mut *guard);
            ctx.push_frame();
            let res = f(ctx);
            ctx.pop_frame();
            res
        }
    }
}
