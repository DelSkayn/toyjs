use vm::Gc;

use crate::Ctx;
use std::string::String as StdString;

#[derive(Clone, Copy)]
pub struct String<'js> {
    pub(crate) ctx: Ctx<'js>,
    pub(crate) ptr: Gc<StdString>,
}

impl<'js> String<'js> {
    pub(crate) unsafe fn wrap(ctx: Ctx<'js>, ptr: Gc<StdString>) -> Self {
        String { ctx, ptr }
    }

    pub fn into_string(self) -> StdString {
        (*self.ptr).clone()
    }
}
