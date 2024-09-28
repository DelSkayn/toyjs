#![allow(clippy::new_without_default)]
// TODO remove once ready to release
#![allow(dead_code)]
#![allow(clippy::missing_safety_doc)]

use std::rc::Rc;

use atom::AtomMap;
use gc::{Arena, Free, OwnedRoot, RootState, Rooted, Trace};

mod atom;
mod gc;
mod object;
mod util;
mod value;

use object::{GcObject, GcShape};

pub enum Error {
    Gc(gc::Error),
}

pub struct Vm<R: RootState> {
    root_shape: GcShape<R>,
    arena: Arena,
    atoms: AtomMap,
}

unsafe impl<R: RootState> Trace for Vm<R> {
    type Free<'a> = Vm<Free<'a>>;
    type Rooted = Vm<Rooted>;

    const NEEDS_TRACE: bool = true;

    fn trace(&self, marker: &gc::Marker) -> Result<(), gc::Error> {
        marker.mark(&self.root_shape)
    }
}

pub struct Context<R: RootState> {
    runtime: Rc<OwnedRoot<Vm<Rooted>>>,
    global: GcObject<R>,
}
