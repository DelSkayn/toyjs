use bitflags::bitflags;

use crate::{
    gc::{self, Gc, RootState, Trace},
    value::Value,
};

bitflags! {
    #[derive(Clone,Copy,Debug,Eq, PartialEq, Hash)]
    pub struct PropertyFlags: u8{
        const ENUMERABLE   = 0b0001;
        const WRITABLE     = 0b0010;
        const CONFIGURABLE = 0b0100;
        const ACCESSOR     = 0b1000;
    }
}

impl PropertyFlags {
    const ORDINARY: Self = Self::ENUMERABLE
        .union(Self::WRITABLE)
        .union(Self::CONFIGURABLE);
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct SlotIdx(u32);

unsafe impl Trace for SlotIdx {
    type Free<'a> = SlotIdx;
    type Rooted = SlotIdx;

    const NEEDS_TRACE: bool = false;

    fn trace(&self, marker: &gc::Marker) -> Result<(), gc::Error> {
        Ok(())
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct PropertyDescriptor {
    flags: PropertyFlags,
    idx: SlotIdx,
}

pub type GcPropertySlots<R> = Gc<R, PropertySlots<R>>;
pub struct PropertySlots<R: RootState> {
    slots: [Value<R>; 0],
}

/*
impl PropertyFlags {
    pub const WRITABLE: PropertyFlags = PropertyFlags(0b1);
    pub const ENUMERABLE: PropertyFlags = PropertyFlags(0b10);
    pub const CONFIGURABLE: PropertyFlags = PropertyFlags(0b100);
    const ACCESSOR: PropertyFlags = PropertyFlags(0b1000);

    pub const BUILTIN: PropertyFlags = PropertyFlags(Self::WRITABLE.0 | Self::CONFIGURABLE.0);

    pub const fn empty() -> Self {
        PropertyFlags(0)
    }

    pub const fn ordinary() -> Self {
        PropertyFlags(0b111)
    }

    #[inline]
    pub const fn contains(self, other: Self) -> bool {
        self.0 & other.0 == other.0
    }

    #[inline]
    pub const fn clear(self, other: Self) -> Self {
        Self(self.0 & !other.0)
    }
}*/
