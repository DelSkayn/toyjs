#![allow(clippy::new_without_default)]
// TODO remove once ready to release
#![allow(dead_code)]
#![allow(clippy::missing_safety_doc)]

use bc::ByteCode;
use common::{interner::Interner, string::StringId};
use dreck::{Gc, Trace};
use object::GcObject;

pub mod exec;
pub mod object;
pub mod stack;
pub mod value;

pub struct Error;

pub trait Compiler {
    fn compile(&self, source: String, context: Option<()>) -> Result<ByteCode, Error>;
}

pub type GcVm<'gc, 'own> = Gc<'gc, 'own, Vm>;
pub struct Vm {
    // A list of jobs to be executed,
    jobs: (),
    // Interned strings.
    interner: Interner<String, StringId>,

    compiler: Option<Box<dyn Compiler>>,
}

impl Vm {
    pub fn new() -> Self {
        Vm {
            jobs: (),
            interner: Interner::new(),
            compiler: None,
        }
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

unsafe impl<'own> Trace<'own> for Vm {
    type Gc<'r> = Vm;

    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        false
    }

    fn trace(&self, _marker: dreck::Marker<'own, '_>) {}
}

pub type GcContext<'gc, 'own> = Gc<'gc, 'own, Context<'gc, 'own>>;
pub struct Context<'gc, 'own> {
    /// The global object,
    global: GcObject<'gc, 'own>,
    /// The vm this context belongs to.
    vm: GcVm<'gc, 'own>,
    // A list of builtin functions
    //builtins: [GcObject<'gc,'own>; <bc::Builtin as Contiguous>::MAX_VALUE];
}

unsafe impl<'gc, 'own> Trace<'own> for Context<'gc, 'own> {
    type Gc<'r> = Context<'r, 'own>;

    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, marker: dreck::Marker<'own, '_>) {
        marker.mark(self.global);
        marker.mark(self.vm);
    }
}

impl<'gc, 'own> Context<'gc, 'own> {
    pub fn new(global: GcObject<'gc, 'own>, vm: GcVm<'gc, 'own>) -> Self {
        Context { global, vm }
    }
}
