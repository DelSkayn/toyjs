//! Pointer types wrapped for usages and providing polyfills for functions not yet available in
//! stable.

use std::{
    marker::PhantomData,
    num::NonZeroUsize,
    ptr::{self, NonNull},
};

#[macro_export]
macro_rules! map_ptr {
    ($ty:ty $(,$field:ident)*) => {
        |x| {
            let ptr = x;
            $(let ptr = std::ptr::addr_of_mut!((*ptr).$field);)*
            ptr
        }
    };
}

/// Pointer with no real owner semantics.
#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct Raw<T: ?Sized> {
    ptr: NonNull<T>,
    _marker: PhantomData<T>,
}

impl<T> Copy for Raw<T> {}
impl<T> Clone for Raw<T> {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl<T> Raw<T> {
    pub fn into_ref<'a>(self) -> Ref<'a, T> {
        Ref {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }

    pub fn into_mut<'a>(self) -> Mut<'a, T> {
        Mut {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }

    pub fn dangling() -> Self {
        Raw {
            ptr: NonNull::dangling(),
            _marker: PhantomData,
        }
    }

    pub unsafe fn write(&self, v: T) {
        self.ptr.as_ptr().write(v);
    }

    pub unsafe fn read(&self) -> T {
        self.ptr.as_ptr().read()
    }

    pub unsafe fn fill(&mut self, value: u8) {
        ptr::write_bytes(self.as_ptr(), value, 1)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ref<'a, T: ?Sized> {
    ptr: NonNull<T>,
    _marker: PhantomData<&'a T>,
}

impl<'a, T: ?Sized> Ref<'a, T> {
    pub fn into_raw(self) -> Raw<T> {
        Raw {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }

    pub unsafe fn into_ref(self) -> &'a T {
        &(*self.ptr.as_ref())
    }
}

impl<'a, T> Ref<'a, T> {
    pub unsafe fn read(&self) -> T {
        self.ptr.as_ptr().read()
    }
}

impl<'a, T: ?Sized> From<&'a T> for Ref<'a, T> {
    fn from(value: &'a T) -> Self {
        Ref {
            ptr: NonNull::from(value),
            _marker: PhantomData,
        }
    }
}

impl<'a, T: ?Sized> From<&'a mut T> for Ref<'a, T> {
    fn from(value: &'a mut T) -> Self {
        Ref {
            ptr: NonNull::from(value),
            _marker: PhantomData,
        }
    }
}

impl<'a, T: ?Sized> From<Mut<'a, T>> for Ref<'a, T> {
    fn from(value: Mut<'a, T>) -> Self {
        Ref {
            ptr: value.ptr,
            _marker: PhantomData,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct Mut<'a, T: ?Sized> {
    ptr: NonNull<T>,
    _marker: PhantomData<&'a mut T>,
}

impl<'a, T: ?Sized> Mut<'a, T> {
    pub fn into_owned(self) -> Raw<T> {
        Raw {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }

    pub fn into_ref(self) -> Ref<'a, T> {
        Ref {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }

    pub fn reborrow(&self) -> Ref<T> {
        Ref {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl<'a, T> Mut<'a, T> {
    pub unsafe fn write(&self, t: T) {
        self.ptr.as_ptr().write(t)
    }

    pub unsafe fn read(&self) -> T {
        self.ptr.as_ptr().read()
    }
}

impl<'a, T> From<&'a mut T> for Mut<'a, T> {
    fn from(value: &'a mut T) -> Self {
        Mut {
            ptr: NonNull::from(value),
            _marker: PhantomData,
        }
    }
}

macro_rules! impl_base_methods {
    ($ty:ident<$($lt:lifetime,)?$gen:ident>) => {
        impl<$($lt,)?$gen : ?Sized> $ty<$($lt,)?$gen> {
            pub fn from_nonnull(ptr: NonNull<$gen>) -> Self {
                Self {
                    ptr,
                    _marker: PhantomData,
                }
            }

            pub fn from_ptr(ptr: *mut $gen) -> Option<Self> {
                NonNull::new(ptr).map(|x| Self {
                    ptr: x,
                    _marker: PhantomData,
                })
            }

            pub unsafe fn from_ptr_unchecked(ptr: *mut $gen) -> Self {
                Self {
                    ptr: NonNull::new_unchecked(ptr),
                    _marker: PhantomData,
                }
            }

            pub fn into_ptr(self) -> *mut $gen {
                self.ptr.as_ptr()
            }

            pub fn as_ptr(&self) -> *mut $gen {
                self.ptr.as_ptr()
            }

            pub fn into_nonnull(self) -> NonNull<$gen> {
                self.ptr
            }

            pub fn as_nonnull(&self) -> NonNull<$gen> {
                self.ptr
            }

            pub unsafe fn as_ref(&self) -> &$gen {
                self.ptr.as_ref()
            }

            pub unsafe fn as_mut(&mut self) -> &mut $gen {
                self.ptr.as_mut()
            }


            pub unsafe fn map_ptr<F,R>(self, f: F) -> $ty<$($lt,)?R>
                where F: FnOnce(*mut T) -> *mut R
            {
                $ty::from_ptr_unchecked(f(self.as_ptr()))
            }


        }

        impl<$($lt,)?$gen> $ty<$($lt,)?$gen> {
            pub fn cast<R>(self) -> $ty<$($lt,)?R>{
                $ty{
                    ptr: self.ptr.cast(),
                    _marker: PhantomData,
                }
            }

            pub fn addr(self) -> NonZeroUsize{
                #[cfg(feature = "nightly")]
                {
                    unsafe{ NonZeroUsize::new_unchecked(self.ptr.as_ptr().addr()) }
                }
                #[cfg(not(feature = "nightly"))]
                {
                    unsafe{ NonZeroUsize::new_unchecked(self.ptr.as_ptr() as usize) }
                }
            }

            pub fn expose_addr(self) -> NonZeroUsize{
                #[cfg(feature = "nightly")]
                {
                    unsafe{ NonZeroUsize::new_unchecked(self.ptr.as_ptr().expose_addr()) }
                }
                #[cfg(not(feature = "nightly"))]
                {
                    unsafe{ NonZeroUsize::new_unchecked(self.ptr.as_ptr() as usize) }
                }
            }

            pub fn from_exposed_addr(addr: NonZeroUsize) -> Self{
                #[cfg(feature = "nightly")]
                {
                    Self{
                        ptr: std::ptr::with_exposed_provenance(addr),
                        _marker: PhantomData,
                    }
                }
                #[cfg(not(feature = "nightly"))]
                {

                    Self{
                        ptr: unsafe{ NonNull::new_unchecked(addr.get() as *mut $gen) },
                        _marker: PhantomData,
                    }
                }
            }

            pub fn with_addr(self, addr: NonZeroUsize) -> Self{
                #[cfg(feature = "nightly")]
                {
                    unsafe{ Self::from_ptr_unchecked(self.ptr.as_ptr().with_addr()) }
                }
                #[cfg(not(feature = "nightly"))]
                {
                    unsafe{ Self::from_ptr_unchecked(addr.get() as *mut $gen) }
                }
            }

            pub fn map_addr<F>(self, f: F) -> Self
            where F: FnOnce(NonZeroUsize) -> NonZeroUsize
            {
                #[cfg(feature = "nightly")]
                {
                    unsafe{
                        Self::from_ptr_unchecked(self.ptr.as_ptr().map_addr(|x| {
                        f(NonZeroUsize::new_unchecked(x)).get()
                    }))
                    }
                }
                #[cfg(not(feature = "nightly"))]
                {
                    Self::from_exposed_addr(f(self.expose_addr()))
                }
            }

            pub unsafe fn map_addr_unchecked<F>(self, f: F) -> Self
            where F: FnOnce(usize) -> usize
            {
                #[cfg(feature = "nightly")]
                {
                    unsafe{
                        Self::from_ptr_unchecked(self.ptr.as_ptr().map_addr(f))
                    }
                }
                #[cfg(not(feature = "nightly"))]
                {
                    Self::from_exposed_addr(NonZeroUsize::new_unchecked(f(self.expose_addr().get())))
                }
            }

            pub unsafe fn add(self, offset: usize) -> Self{
                self.map_addr(|x| NonZeroUsize::new_unchecked(x.get() + offset))
            }

            pub unsafe fn sub(self, offset: usize) -> Self{
                self.map_addr(|x| NonZeroUsize::new_unchecked(x.get() - offset))
            }

            pub unsafe fn offset(self, offset: isize) -> Self{
                self.map_addr(|x| NonZeroUsize::new_unchecked((x.get() as isize + offset) as usize))
            }

        }
    };
}

impl_base_methods!(Ref<'a, T>);
impl_base_methods!(Mut<'a, T>);
impl_base_methods!(Raw<T>);
