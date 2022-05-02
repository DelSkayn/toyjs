use vm::Gc;

use crate::Ctx;
use std::{fmt, string::String as StdString};

#[derive(Clone, Copy)]
pub struct String<'js> {
    pub(crate) ctx: Ctx<'js>,
    pub(crate) ptr: Gc<StdString>,
}

impl<'js> fmt::Display for String<'js> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.ptr.as_str().fmt(f)
    }
}

impl<'js> String<'js> {
    pub(crate) unsafe fn wrap(ctx: Ctx<'js>, ptr: Gc<StdString>) -> Self {
        String { ctx, ptr }
    }

    pub fn as_str(&self) -> &str {
        self.ptr.as_str()
    }

    pub fn into_string(self) -> StdString {
        (*self.ptr).clone()
    }
}
