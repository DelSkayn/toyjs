#![feature(allocator_api)]
#![allow(dead_code)]
#![allow(clippy::new_without_default)]

use std::mem;

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
use vm::{realm::RealmBox, vm::Vm};

pub mod convert;

mod runtime;

#[derive(Clone)]
pub struct ToyJs {
    vm: Vm,
}

impl ToyJs {
    pub fn new() -> ToyJs {
        ToyJs { vm: Vm::new() }
    }

    pub fn collect_debt(&self) {
        self.vm.collect_debt()
    }

    pub fn collect_full(&self) {
        self.vm.collect_full()
    }
}

pub struct Context {
    vm: Vm,
    inner: RealmBox,
}

impl Context {
    pub fn new(js: &ToyJs) -> Self {
        let res = Context {
            vm: js.vm.clone(),
            inner: vm::Realm::new_with_user_data(&js.vm, UserData::new()),
        };
        res.with(runtime::init);
        res
    }

    pub fn with<F, R>(&self, f: F) -> R
    where
        F: for<'js> FnOnce(Ctx<'js>) -> R,
    {
        let guard = self.vm.lock();
        let res = unsafe {
            let ptr = self.inner.as_ptr();
            (*ptr).user_enter_frame(0);
            let ctx = Ctx::wrap(self.inner.as_ptr());
            let res = f(ctx);
            (*ptr).user_pop_frame();
            res
        };
        mem::drop(guard);
        res
    }
}
