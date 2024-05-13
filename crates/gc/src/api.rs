use crate::marker::{Invariant, Owner};

pub trait GcImpl{
    type Arena: ArenaImpl;
    type Trace: TraceImpl<Marker = Self::Marker>;
    type Marker: MarkerImpl<Ptr = Self::Ptr>;
    type Ptr;
}

pub trait TraceImpl{
    type Marker;
}

pub trait MarkerImpl{
    type Ptr;
}

pub struct MarkerApi<'own, 'm, I: GcImpl>(I::Marker);

/// A trait for a type which can be GC allocated. It essential that this trait is implemented
/// correctly for safe use of this library.
///
/// # Safety
/// TODO
pub unsafe trait Trace<'own, I> 
where I: TraceImpl
{
    /// The type with a different gc lifetime.
    type Gc<'r>
    where
        Self: Sized;

    /// Wether this object can contain other GC pointers and thus needs to be traced.
    ///
    /// It is safe to return true it the implementing object contains no pointers but this function
    /// must never return false if it could contain pointers.
    fn needs_trace() -> bool
    where
        Self: Sized;

    /// Trace the object marking all GC pointers contained in the implementing object.
    fn trace(&self, marker: MarkerApi<'own, '_,I>);

    /// An object for changing the Gc lifetime of a gc allocated object.
    /// This is essentially [`std::mem::transmute`] but only for a single lifetime.
    /// Should compile to down to nothing
    #[inline(always)]
    unsafe fn rebind<'gc>(self) -> Self::Gc<'gc>
    where
        Self: Sized,
    {
        use std::mem::ManuallyDrop;
        union Transmute<T, U> {
            a: ManuallyDrop<T>,
            b: ManuallyDrop<U>,
        }
        //TODO: compiler error using static assertions?
        assert_eq!(
            std::mem::size_of::<Self>(),
            std::mem::size_of::<Self::Gc<'gc>>(),
            "type `{}` implements rebind but its `Rebound` ({}) is a different size",
            std::any::type_name::<Self>(),
            std::any::type_name::<Self::Gc<'gc>>(),
        );

        ManuallyDrop::into_inner(
            (Transmute {
                a: ManuallyDrop::new(self),
            })
            .b,
        )
    }
}


pub trait ArenaImpl{
    pub fn new() -> Self;
}

pub struct ArenaApi<'own, I> {
    inner: I,
    marker: Invariant<'own>,
}

impl<'own, I: ArenaImpl> ArenaApi<'own, I> {
    pub unsafe fn new(owner: &Owner<'own>) -> Self {
        ArenaApi {
            inner: I::new(),
            marker: Invariant::new(),
        }
    }

    pub fn add<'gc, T: Trace<'own, I>>(&mut self, t: T) -> I::Gc
}
