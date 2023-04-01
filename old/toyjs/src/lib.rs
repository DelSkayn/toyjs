use lock::Lock;
use vm::{
    atom::Atoms,
    gc::{Gc, Owner, Root},
};

mod lock;

struct Inner {
    owner: Owner<'static>,
    root: Root<'static>,
    atoms: Gc<'static, 'static, Atoms<'static, 'static>>,
}

pub struct ToyJs(Lock<Inner>);
