use common::{hashmap::HashMap, ptr::Ref};

use super::{cell::TraceImpl, Free, Gc, Marker, RootState, Rooted};
use crate::{atom::Atom, gc::Error};

pub unsafe trait Trace {
    type Free<'a>: Trace;
    type Rooted: Trace;

    const NEEDS_TRACE: bool;

    fn trace(&self, marker: &Marker) -> Result<(), Error>;

    unsafe fn assert_rooted(self) -> Self::Rooted
    where
        Self: Sized,
    {
        assert!(
            const { std::mem::size_of::<Self>() == std::mem::size_of::<Self::Rooted>() },
            "Invalid rooted type definition, size_of<T>() != size_of::<T::Rooted>()"
        );
        assert!(
            const { std::mem::align_of::<Self>() == std::mem::align_of::<Self::Rooted>() },
            "Invalid rooted type definition, align_of<T>() != align_of::<T::Rooted>()"
        );
        let r = unsafe { Ref::from(&self).cast::<Self::Rooted>().read() };
        std::mem::forget(self);
        r
    }

    unsafe fn assert_rooted_ref(&self) -> &Self::Rooted
    where
        Self: Sized,
    {
        assert!(
            const { std::mem::size_of::<Self>() == std::mem::size_of::<Self::Rooted>() },
            "Invalid rooted type definition, size_of<T>() != size_of::<T::Rooted>()"
        );
        assert!(
            const { std::mem::align_of::<Self>() == std::mem::align_of::<Self::Rooted>() },
            "Invalid rooted type definition, align_of<T>() != align_of::<T::Rooted>()"
        );
        Ref::from(self).cast::<Self::Rooted>().into_borrow()
    }

    unsafe fn free_root<'a>(self) -> Self::Free<'a>
    where
        Self: Sized,
    {
        assert!(
            const { std::mem::size_of::<Self>() == std::mem::size_of::<Self::Rooted>() },
            "Invalid rooted type definition, size_of<T>() != size_of::<T::Rooted>()"
        );
        assert!(
            const { std::mem::align_of::<Self>() == std::mem::align_of::<Self::Rooted>() },
            "Invalid rooted type definition, align_of<T>() != align_of::<T::Rooted>()"
        );
        let r = unsafe { Ref::from(&self).cast::<Self::Free<'a>>().read() };
        std::mem::forget(self);
        r
    }
}

unsafe impl Trace for Atom {
    type Free<'a> = Atom;
    type Rooted = Atom;

    const NEEDS_TRACE: bool = true;

    fn trace(&self, marker: &Marker) -> Result<(), Error> {
        marker.mark_atom(*self)
    }
}

unsafe impl Trace for String {
    type Free<'a> = Atom;
    type Rooted = Atom;

    const NEEDS_TRACE: bool = false;

    fn trace(&self, _marker: &Marker) -> Result<(), Error> {
        Ok(())
    }
}

unsafe impl<K: Trace, V: Trace> Trace for HashMap<K, V> {
    type Free<'a> = HashMap<K::Free<'a>, V::Free<'a>>;
    type Rooted = HashMap<K::Rooted, V::Rooted>;

    const NEEDS_TRACE: bool = K::NEEDS_TRACE | V::NEEDS_TRACE;

    fn trace(&self, marker: &Marker) -> Result<(), Error> {
        if K::NEEDS_TRACE && V::NEEDS_TRACE {
            for (k, v) in self.iter() {
                k.trace(marker)?;
                v.trace(marker)?;
            }
        } else if K::NEEDS_TRACE {
            for k in self.keys() {
                k.trace(marker)?;
            }
        } else if V::NEEDS_TRACE {
            for v in self.values() {
                v.trace(marker)?;
            }
        }
        Ok(())
    }
}
