use std::{
    any::Any,
    collections::{HashMap, HashSet},
    hint::unreachable_unchecked,
};

use bc::{ByteCode, FunctionId};
use dreck::{Gc, Trace};

use crate::value::Value;

mod props;

pub struct BcFunction<'gc, 'own> {
    pub id: FunctionId,
    pub bc: Gc<'gc, 'own, ByteCode>,
}

pub type NativeFunc<'gc, 'own> = Box<dyn Fn() -> Result<Value<'gc, 'own>, Value<'gc, 'own>>>;

pub trait UserData<'own>: Trace<'own> + Any {}

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
    UserData(Box<dyn UserData<'own>>),
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
            ObjectSlot::UserData(_) => todo!(),
        }
    }
}

pub enum Property<'gc, 'own> {
    Value(Value<'gc, 'own>),
    Accessor {
        get: Value<'gc, 'own>,
        set: Value<'gc, 'own>,
    },
}

unsafe impl<'gc, 'own> Trace<'own> for Property<'gc, 'own> {
    type Gc<'r> = Property<'r, 'own>;

    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, marker: dreck::Marker<'own, '_>) {
        match self {
            Property::Value(ref x) => x.trace(marker),
            Property::Accessor { ref get, ref set } => {
                get.trace(marker);
                set.trace(marker);
            }
        }
    }
}

pub struct Properties<'gc, 'own> {
    values: HashMap<String, Property<'gc, 'own>>,
}

unsafe impl<'gc, 'own> Trace<'own> for Properties<'gc, 'own> {
    type Gc<'r> = Properties<'r, 'own>;

    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, marker: dreck::Marker<'own, '_>) {
        for v in self.values.values() {
            v.trace(marker)
        }
    }
}

impl<'gc, 'own> Properties<'gc, 'own> {
    pub fn new() -> Self {
        Properties {
            values: HashMap::new(),
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
    properties: Properties<'gc, 'own>,
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
        self.slot.trace(marker);
        self.prototype.trace(marker);
    }
}

impl<'gc, 'own> Object<'gc, 'own> {
    pub fn new() -> Self {
        Object {
            shape: (),
            prototype: None,
            entries: (),
            properties: Properties::new(),
            slot: ObjectSlot::Generic,
        }
    }

    pub fn entry_function(bc: Gc<'gc, 'own, ByteCode>) -> Self {
        Object {
            shape: (),
            prototype: None,
            entries: (),
            properties: Properties::new(),
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

impl<'gc, 'own> Default for Object<'gc, 'own> {
    fn default() -> Self {
        Self::new()
    }
}
