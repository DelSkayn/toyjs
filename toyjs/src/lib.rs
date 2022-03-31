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
use vm::{realm::RealmBox, Vm};
pub mod error;
pub use error::{Error, Result};

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
        self.vm.collect_debt();
    }

    pub fn collect_full(&self) {
        self.vm.collect_full();
    }
}

pub struct Context {
    vm: Vm,
    realm: RealmBox,
}

impl Context {
    pub fn new(js: &ToyJs) -> Self {
        let res = Context {
            vm: js.vm.clone(),
            realm: vm::Realm::new_with_user_data(&js.vm, UserData::new()),
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
            let _vm = &*guard;
            let ptr = self.realm.as_ptr();
            (*ptr).user_enter_frame(0);
            let ctx = Ctx::wrap(ptr);
            let res = f(ctx);
            (*ptr).user_pop_frame();
            res
        };
        #[allow(clippy::drop_ref)]
        mem::drop(guard);
        res
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe { self.realm.free() }
    }
}
