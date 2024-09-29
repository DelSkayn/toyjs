#![allow(clippy::new_without_default)]
// TODO remove once ready to release
#![allow(dead_code)]
#![allow(clippy::missing_safety_doc)]

use std::{cell::RefCell, pin::Pin, rc::Rc};

use gc::{Arena, Free, OwnedRoot, OwnedRooted, OwnedTrace, RootState, Rooted, Trace};
use pin_project_lite::pin_project;

pub mod atom;
pub mod gc;
pub mod object;
pub mod util;
pub mod value;

use object::{GcObject, GcShape, Shape};

#[derive(Debug)]
pub enum Error {
    Gc(gc::Error),
}

type VmRef = Pin<Rc<RefCell<Vm>>>;

pin_project! {
    pub struct Vm {
        #[pin]
        arena: Arena,
        #[pin]
        data: OwnedRoot<VmData<OwnedRooted>>,
    }
}

pub struct VmData<R: RootState> {
    root_shape: GcShape<R>,
}

impl Vm {
    pub unsafe fn new() -> Result<VmRef, Error> {
        let arena = Arena::new();
        let data = OwnedRoot::new();
        let res = Rc::pin(RefCell::new(Vm { arena, data }));

        {
            let mut pin = Pin::new(res.borrow_mut());
            let mut this = pin.as_mut().project();
            Arena::init(this.arena.as_mut());

            let root_shape = this
                .arena
                .as_mut()
                .get_mut()
                .alloc(Shape::root())
                .map_err(Error::Gc)?;

            let vm_data = VmData { root_shape }.free_root();
            gc::root_owned!(this.arena.get_mut(), this.data.as_ref(), vm_data);
        }

        Ok(res)
    }
}

unsafe impl<R: RootState> Trace for VmData<R> {
    type Free<'a> = VmData<Free<'a>>;
    type Rooted = VmData<Rooted>;

    const NEEDS_TRACE: bool = true;

    fn trace(&self, marker: &gc::Marker) -> Result<(), gc::Error> {
        marker.mark(&self.root_shape)
    }
}
unsafe impl<R: RootState> OwnedTrace for VmData<R> {
    type Owned = VmData<OwnedRooted>;
}

pub struct Context<R: RootState> {
    runtime: VmRef,
    global: GcObject<R>,
}
