use std::{fmt, string::String as StdString};

use vm::{cell::CellOwner, gc::Gc};

use crate::Ctx;

#[derive(Clone, Copy)]
pub struct String<'js> {
    pub(crate) ptr: Gc<'js, 'js, StdString>,
    pub(crate) ctx: Ctx<'js>,
}

impl<'js> fmt::Display for String<'js> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let owner = unsafe { CellOwner::new(self.ctx.id) };
        self.ptr.borrow(&owner).as_str().fmt(f)
    }
}

impl<'js> String<'js> {
    pub(crate) fn from_vm(ctx: Ctx<'js>, ptr: Gc<'_, 'js, StdString>) -> Self {
        unsafe {
            ctx.context.root.push(ptr);
            String {
                ctx,
                ptr: vm::gc::rebind(ptr),
            }
        }
    }

    pub(crate) fn into_vm(self) -> Gc<'js, 'js, StdString> {
        self.ptr
    }

    pub fn as_str(&self) -> &'js str {
        unsafe {
            let owner = CellOwner::new(self.ctx.id);
            self.ptr.borrow(std::mem::transmute(&owner)).as_str()
        }
    }

    pub fn into_string(self) -> StdString {
        let owner = unsafe { CellOwner::new(self.ctx.id) };
        self.ptr.borrow(&owner).clone()
    }
}
