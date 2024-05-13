//! A set of zero-sized marker types.

use std::marker::PhantomData;

/// A marker struct which marks a lifetime as invariant.
///
// Since 'inv required to be both contravariant as well as covariant the result is an invariant
// lifetime.
#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, Debug, Default)]
pub struct Invariant<'inv>(PhantomData<&'inv mut &'inv fn(&'inv ()) -> &'inv ()>);

impl<'inv> Invariant<'inv> {
    pub fn new() -> Self {
        Invariant(PhantomData)
    }

    pub fn new_ref<T>(_v: &'inv T) -> Self {
        Invariant(PhantomData)
    }
}

/// A marker struct which marks a lifetime as covariant.
#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, Debug, Default)]
pub struct Covariant<'co>(PhantomData<&'co ()>);

impl<'co> Covariant<'co> {
    pub fn new() -> Self {
        Covariant(PhantomData)
    }

    pub fn new_ref<T>(_v: &'co T) -> Self {
        Covariant(PhantomData)
    }
}

/// An struct which acts as the owner of garbage collected values.
///
/// Altough this struct is zero-sized it acts as if all GC values are contained within this object
/// allowing zero-cost, statically verified, safe mutable access to shared GC pointers.
///
/// Using this owner to borrow a GC allocated value mutably also borrows the owner mutably for the
/// same lifetime, thus disallowing any GC pointer for being borrowed immutably. For the use of
/// this object see [`Gc::borrow`](`crate::Gc::borrow`) and [`Gc::borrow_mut`](`crate::Gc::borrow_mut`).
#[derive(Debug)]
pub struct Owner<'own>(Invariant<'own>);

impl<'own> Owner<'own> {
    /// Create a new owner.
    ///
    /// This function is unsafe as it would allow one to create two owners with the same lifetime
    /// which would allow one to violate the borrow mechanism this marker is designed for.
    ///
    /// Instead use the safe macros to create an owner.
    pub unsafe fn new() -> Self {
        Owner(Invariant::new())
    }

    /// Create a new owner.
    ///
    /// This function is unsafe as it would allow one to create two owners with the same lifetime
    /// which would allow one to violate the borrow mechanism this marker is designed for.
    ///
    /// Instead use the safe macros to create an owner.
    pub unsafe fn from_invariant(inv: Invariant<'own>) -> Self {
        Owner(inv)
    }
}
