use common::atom::Atoms;

use crate::{Ctx, Result};

#[derive(Debug)]
pub struct Atom<'js> {
    pub(crate) atom: common::atom::Atom,
    pub(crate) ctx: Ctx<'js>,
}

impl<'js> Drop for Atom<'js> {
    fn drop(&mut self) {
        self.ctx.context.atoms.decrement(self.atom);
    }
}

pub trait IntoAtom<'js> {
    fn into_atom(self, ctx: Ctx<'js>) -> Result<'js, Atom<'js>>;
}

impl<'js> IntoAtom<'js> for &str {
    fn into_atom(self, ctx: Ctx<'js>) -> Result<'js, Atom<'js>> {
        Ok(Atom {
            atom: ctx.context.atoms.atomize_string(self),
            ctx,
        })
    }
}

impl<'js> IntoAtom<'js> for String {
    fn into_atom(self, ctx: Ctx<'js>) -> Result<'js, Atom<'js>> {
        Ok(Atom {
            atom: ctx.context.atoms.atomize_string(&self),
            ctx,
        })
    }
}

impl<'js> IntoAtom<'js> for i32 {
    fn into_atom(self, ctx: Ctx<'js>) -> Result<'js, Atom<'js>> {
        Ok(Atom {
            atom: Atoms::try_atomize_int(self)
                .unwrap_or_else(|| ctx.context.atoms.atomize_string(&self.to_string())),
            ctx,
        })
    }
}

impl<'js> IntoAtom<'js> for f64 {
    fn into_atom(self, ctx: Ctx<'js>) -> Result<'js, Atom<'js>> {
        let atom = if self as i32 as f64 == self {
            Atoms::try_atomize_int(self as i32)
                .unwrap_or_else(|| ctx.context.atoms.atomize_string(&self.to_string()))
        } else {
            ctx.context.atoms.atomize_string(&self.to_string())
        };

        Ok(Atom { atom, ctx })
    }
}
