//! An offset helper type for preventing errors when dealing pointer offset of different types.

use std::{
    alloc::Layout,
    cmp::{Ord, Ordering, PartialEq, PartialOrd},
    marker, mem, ops,
};

pub trait OffsetExt {
    type Out;
    fn offset_to(self, other: Self) -> Offset<Self::Out>;

    fn apply(self, offset: Offset<Self::Out>) -> Self;
}

impl<T> OffsetExt for *mut T {
    type Out = T;

    #[inline(always)]
    fn offset_to(self, other: Self) -> Offset<Self::Out> {
        let s = self as usize;
        let o = other as usize;
        debug_assert!(s < o);
        Offset {
            offset: o - s,
            _marker: marker::PhantomData,
        }
    }

    #[inline(always)]
    fn apply(self, offset: Offset<Self::Out>) -> Self {
        (self as usize + offset.offset) as *mut _
    }
}

impl<T> OffsetExt for *const T {
    type Out = T;

    #[inline(always)]
    fn offset_to(self, other: Self) -> Offset<Self::Out> {
        let s = self as usize;
        let o = other as usize;
        debug_assert!(s < o);
        Offset {
            offset: o - s,
            _marker: marker::PhantomData,
        }
    }

    #[inline(always)]
    fn apply(self, offset: Offset<Self::Out>) -> Self {
        (self as usize + offset.offset) as *const _
    }
}

#[derive(Clone, Copy)]
pub struct Offset<T> {
    offset: usize,
    _marker: marker::PhantomData<T>,
}
impl<T> Eq for Offset<T> {}

impl<T> PartialEq for Offset<T> {
    #[inline(always)]
    fn eq(&self, other: &Offset<T>) -> bool {
        self.offset.eq(&other.offset)
    }
}

impl<T> PartialOrd for Offset<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.offset.partial_cmp(&other.offset)
    }
}

impl<T> Ord for Offset<T> {
    #[inline(always)]
    fn cmp(&self, other: &Self) -> Ordering {
        self.offset.cmp(&other.offset)
    }

    /*
    fn clamp(self, min: Self, max: Self) -> Self
    where
        Self: Sized,
    {
        Offset {
            offset: self.offset.clamp(min.offset, max.offset),
            __marker: PhantomData,
        }
    }
    */
}

impl<T> Offset<T> {
    pub const fn new(offset: usize) -> Offset<T> {
        Offset {
            offset,
            _marker: marker::PhantomData,
        }
    }

    pub const fn zero() -> Offset<T> {
        Offset {
            offset: 0,
            _marker: marker::PhantomData,
        }
    }

    /// Cast the offset to a different type
    /// Offsets are stored as a number of values.
    /// Casting between types thus can changes the number values that fit in this size.
    #[inline(always)]
    pub const fn cast<U>(self) -> Offset<U> {
        let offset = self.offset / mem::size_of::<T>() * mem::size_of::<U>();
        Offset {
            offset,
            _marker: marker::PhantomData,
        }
    }

    /// Apply the offset to a pointer
    #[inline(always)]
    pub fn apply_to(self, ptr: *mut T) -> *mut T {
        (ptr as usize + self.offset) as *mut T
    }

    /// Apply the offset to a const pointer
    #[inline(always)]
    pub fn apply_to_const(self, ptr: *const T) -> *const T {
        (ptr as usize + self.offset) as *const T
    }

    /// Returns an offset of with an offset with the next power of two as a size.
    #[inline(always)]
    pub fn next_power_of_two(self) -> Offset<T> {
        Offset {
            offset: self.offset.next_power_of_two(),
            _marker: marker::PhantomData,
        }
    }

    /// Returns a layout from the offset aligned by its type.
    pub fn to_layout(self) -> Layout {
        Layout::from_size_align(self.offset * mem::size_of::<T>(), mem::align_of::<T>()).unwrap()
    }

    /// The returns the number of values that fit in the offset.
    #[inline(always)]
    pub const fn as_number(self) -> usize {
        self.offset
    }

    /// Returns the number of bytes that fit in the offset.
    #[inline(always)]
    pub const fn as_size_bytes(self) -> usize {
        self.offset * mem::size_of::<T>()
    }
}

impl<T> ops::Add for Offset<T> {
    type Output = Offset<T>;

    fn add(mut self, rhs: Self) -> Self::Output {
        self.offset += rhs.offset;
        self
    }
}

impl<T> ops::Sub for Offset<T> {
    type Output = Offset<T>;

    fn sub(mut self, rhs: Self) -> Self::Output {
        self.offset -= rhs.offset;
        self
    }
}
