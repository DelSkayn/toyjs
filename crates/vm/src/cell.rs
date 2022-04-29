use core::fmt;
use std::{cell::UnsafeCell, marker::PhantomData};

// -- The Guard from generativity crate

#[doc(hidden)]
#[derive(Copy, Clone)]
pub struct Id<'id>(PhantomData<&'id mut &'id fn(&'id ()) -> &'id ()>);

impl<'id> Id<'id> {
    #[doc(hidden)]
    pub unsafe fn new() -> Self {
        Id(PhantomData)
    }
}

impl<'id> fmt::Debug for Id<'id> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Id<'id>").finish()
    }
}

pub struct Guard<'id>(Id<'id>);

impl<'id> Guard<'id> {
    pub unsafe fn new(id: Id<'id>) -> Self {
        Guard(id)
    }
}

impl<'id> fmt::Debug for Guard<'id> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Guard<'id>").finish()
    }
}

/// Create a new cell owner.
#[macro_export]
macro_rules! new_cell_owner {
    ($name:ident) => {
        let tag = unsafe { $crate::cell::Id::new() };
        let _cell_owner;
        #[allow(unused_mut)]
        let mut $name = unsafe { $crate::cell::CellOwner::new(tag) };
        {
            if false {
                #[allow(non_camel_case_types)]
                struct new_cell_owner<'id>(&'id $crate::cell::Id<'id>);
                impl<'id> ::core::ops::Drop for new_cell_owner<'id> {
                    fn drop(&mut self) {}
                }
                _cell_owner = new_cell_owner(&tag);
            }
        }
    };
}

// -- The lcell from qcell crate

/// The owner of all values in [`LCell`]'s with the same lifetime.
pub struct CellOwner<'rt>(Id<'rt>);

impl<'rt> CellOwner<'rt> {
    // Create a new Cell owener
    pub unsafe fn new(id: Id<'rt>) -> Self {
        CellOwner(id)
    }

    /// Shared borrow the value in the cell.
    #[inline(always)]
    pub fn borrow<'a, T: ?Sized>(&'a self, cell: &LCell<'rt, T>) -> &'a T {
        unsafe { &(*cell.value.get()) }
    }

    /// Mutable borrow the value in the cell.
    #[inline(always)]
    pub fn borrow_mut<'a, T: ?Sized>(&'a mut self, cell: &LCell<'rt, T>) -> &'a mut T {
        unsafe { &mut (*cell.value.get()) }
    }
}

/// A cell which can be accessed with a [`CellOwner`] and has no borrow checking overhead.
///
/// # Example
///
/// ```
/// # use toyjs_vm::{new_cell_owner,cell::LCell};
/// new_cell_owner!(owner);
/// let c1 = LCell::new(1);
/// *c1.borrow_mut(&mut owner) += 1;
/// assert_eq!(*c1.borrow(&owner), 2);
///
/// ```
pub struct LCell<'rt, T: ?Sized> {
    _id: Id<'rt>,
    value: UnsafeCell<T>,
}

impl<'rt, T> LCell<'rt, T> {
    #[inline]
    pub fn new(value: T) -> LCell<'rt, T> {
        unsafe {
            LCell {
                _id: Id::new(),
                value: UnsafeCell::new(value),
            }
        }
    }
}

impl<'rt, T: ?Sized> LCell<'rt, T> {
    /// Borrow the contained value.
    pub fn borrow<'a>(&'a self, owner: &'a CellOwner<'rt>) -> &'a T {
        owner.borrow(self)
    }

    /// Mutable borrow the contained value.
    pub fn borrow_mut<'a>(&'a self, owner: &'a mut CellOwner<'rt>) -> &'a mut T {
        owner.borrow_mut(self)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn cell() {
        new_cell_owner!(owner);
        let c1 = LCell::new(1);
        *c1.borrow_mut(&mut owner) += 1;
        assert_eq!(*c1.borrow(&owner), 2);
    }
}
