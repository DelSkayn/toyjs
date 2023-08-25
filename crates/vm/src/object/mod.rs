use bc::{ByteCode, FunctionId};
use dreck::{Gc, Trace};
use std::{
    collections::{HashMap, HashSet},
    hint::unreachable_unchecked,
};

use crate::value::Value;

pub struct BcFunction<'gc, 'own> {
    pub id: FunctionId,
    pub bc: Gc<'gc, 'own, ByteCode>,
}

pub type NativeFunc<'gc, 'own> = Box<dyn Fn() -> Result<Value<'gc, 'own>, Value<'gc, 'own>>>;

pub enum ObjectSlot<'gc, 'own> {
    Generic,
    Array,
    Symbol,
    Error,
    Map(HashMap<Value<'gc, 'own>, Value<'gc, 'own>>),
    Set(HashSet<Value<'gc, 'own>>),
    NativeFunction(NativeFunc<'gc, 'own>),
    BcFunction(BcFunction<'gc, 'own>),
    Proxy(()),
}

unsafe impl<'gc, 'own> Trace<'own> for ObjectSlot<'gc, 'own> {
    type Gc<'r> = Object<'r, 'own>;

    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, marker: dreck::Marker<'own, '_>) {
        match *self {
            ObjectSlot::Generic
            | ObjectSlot::Array
            | ObjectSlot::Symbol
            | ObjectSlot::Error
            | ObjectSlot::NativeFunction(_) => {}
            ObjectSlot::Map(ref x) => x.trace(marker),
            ObjectSlot::Set(ref x) => x.trace(marker),
            ObjectSlot::BcFunction(_) => todo!(),
            ObjectSlot::Proxy(_) => todo!(),
        }
    }
}

pub type GcObject<'gc, 'own> = Gc<'gc, 'own, Object<'gc, 'own>>;
pub struct Object<'gc, 'own> {
    // TODO: hidden class
    shape: (),
    // The prototype for this object.
    prototype: Option<GcObject<'gc, 'own>>,
    // TODO: The values of the object indexed with a integer.
    entries: (),
    // TODO: The values of the object indexed with any non-integer value..
    properties: (),
    // The special value for this object.
    slot: ObjectSlot<'gc, 'own>,
}

unsafe impl<'gc, 'own> Trace<'own> for Object<'gc, 'own> {
    type Gc<'r> = Object<'r, 'own>;

    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, marker: dreck::Marker<'own, '_>) {
        self.slot.trace(marker)
    }
}

impl<'gc, 'own> Object<'gc, 'own> {
    pub fn new() -> Self {
        Object {
            shape: (),
            prototype: None,
            entries: (),
            properties: (),
            slot: ObjectSlot::Generic,
        }
    }

    pub fn entry_function(bc: Gc<'gc, 'own, ByteCode>) -> Self {
        Object {
            shape: (),
            prototype: None,
            entries: (),
            properties: (),
            slot: ObjectSlot::BcFunction(BcFunction {
                id: FunctionId::entry(),
                bc,
            }),
        }
    }

    pub fn bc_function<'a>(&'a self) -> Option<&'a BcFunction<'gc, 'own>> {
        if let ObjectSlot::BcFunction(ref x) = self.slot {
            Some(x)
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// Caller must ensure that the object contains a bc function.
    pub unsafe fn unchecked_bc_function<'a>(&'a self) -> &'a BcFunction<'gc, 'own> {
        if let ObjectSlot::BcFunction(ref x) = self.slot {
            x
        } else {
            unreachable_unchecked()
        }
    }
}
