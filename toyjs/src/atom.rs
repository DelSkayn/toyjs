use std::mem;

use vm::atom::Atom as VmAtom;

use crate::Ctx;

pub struct Atom<'js> {
    ctx: Ctx<'js>,
    atom: VmAtom,
}

impl<'js> Atom<'js> {
    pub(crate) unsafe fn wrap(ctx: Ctx<'js>, atom: VmAtom) -> Self {
        Atom { ctx, atom }
    }

    pub(crate) unsafe fn into_vm(self) -> VmAtom {
        let res = self.atom;
        mem::forget(self);
        res
    }
}

impl<'js> Clone for Atom<'js> {
    fn clone(&self) -> Self {
        unsafe {
            (*self.ctx.ctx).vm().increment(self.atom);
        }
        Atom {
            ctx: self.ctx,
            atom: self.atom,
        }
    }
}

impl<'js> Drop for Atom<'js> {
    fn drop(&mut self) {
        unsafe {
            (*self.ctx.ctx).vm().decrement(self.atom);
        }
    }
}
