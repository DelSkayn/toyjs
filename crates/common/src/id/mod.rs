//! Module for  working with type-safe indexed collections.
//!
//! Instead of indexing into a collection with a usize, which is no more type safe than a void
//! pointer, this module proveds the Id trait allong with some collections and a way to easily
//! create new id's. The collection only provide indexing for the id's this prevent mixin indexes
//! of serveral different collections.
//!
//! Id's are stored as `NonZeroU32` this allows `Option<Id>` to be equal in size as `Id`.

use core::fmt;
use std::{num::NonZeroU32, usize};

pub mod collections;

pub trait Id: Sized + Clone + Copy {
    fn idx(self) -> usize;

    fn from_idx(idx: usize) -> Result<Self, IdRangeError>;
}

impl Id for u32 {
    fn idx(self) -> usize {
        self as usize
    }

    fn from_idx(idx: usize) -> Result<Self, IdRangeError> {
        idx.try_into().map_err(|_| IdRangeError)
    }
}

impl Id for NonZeroU32 {
    fn idx(self) -> usize {
        self.get() as usize
    }

    fn from_idx(idx: usize) -> Result<Self, IdRangeError> {
        if idx > (u32::MAX - 1) as usize {
            return Err(IdRangeError);
        }
        // SAFETY: We check above if the idx is in a valid range so this is safe.
        unsafe { Ok(NonZeroU32::new_unchecked((idx as u32) ^ u32::MAX)) }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IdRangeError;

impl fmt::Display for IdRangeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Id for node exceeded maximum allowed value.")
    }
}

/// A macro which implements a newtype index.
#[macro_export]
macro_rules! id {
    (
        $(#[$m:meta])*
        $vis:vis struct $name:ident $( < $($gen:ident),* $(,)? > )? ) => {

        $(#[$m])*
        $vis struct $name $( < $( $gen, )* > )?{
            id: ::std::num::NonZeroU32,
            $(
                _marker: PhantomData< $($gen),*>
            )?
        }

        impl$( <$($gen),* > )? $crate::id::Id for $name$( <$($gen),* > )?{
            #[inline]
            fn idx(self) -> usize{
                (self.id.get() ^ u32::MAX) as usize
            }

            #[inline]
            fn from_idx(idx: usize) -> ::std::result::Result<Self, $crate::id::IdRangeError>{
                let id = ::std::num::NonZeroU32::from_idx(idx)?;
                Ok(Self{
                    id,
                    $(
                        _marker: PhantomData::< $($gen),* >
                    )?
                })
            }
        }

        impl$( <$($gen),* > )? $name $( < $($gen),* > )? {
            $vis const MIN: Self = unsafe{
                Self{
                        id: ::std::num::NonZeroU32::new_unchecked(u32::MAX),
                        $(
                            _marker: PhantomData::<$($gen),*>,
                        )?
                    }
            };

            $vis const MAX : Self = unsafe{
                Self{
                        id: ::std::num::NonZeroU32::new_unchecked((u32::MAX - 1) ^ u32::MAX),
                        $(
                            _marker: PhantomData::<$($gen),*>,
                        )?
                    }
            };

            #[inline]
            $vis const fn from_u32(index: u32) -> Option<Self> {
                if index > (u32::MAX - 1) {
                    return None;
                }

                unsafe {
                    Some(Self{
                        id: ::std::num::NonZeroU32::new_unchecked(index as u32 ^ u32::MAX),
                        $(
                            _marker: PhantomData::<$($gen),*>,
                        )?
                    })
                }
            }

            #[inline]
            $vis const fn into_u32(self) -> u32 {
                self.id.get() ^ u32::MAX
            }

            #[inline]
            $vis fn next(self) -> Option<Self>{
                Self::from_u32(self.into_u32() + 1)
            }
        }

        impl$( <$($gen),* > )? Clone for $name $( < $($gen),* > )? {
            fn clone(&self) -> Self {
                *self
            }
        }
        impl$( <$($gen),* > )? Copy for $name $( < $($gen),* > )? { }
        impl$( <$($gen),* > )? PartialEq for $name $( < $($gen),* > )? {
            fn eq(&self, other: &Self) -> bool {
                self.id == other.id
            }
        }
        impl$( <$($gen),* > )? Eq for $name $( < $($gen),* > )? { }

        impl$( <$($gen),* > )? PartialOrd for $name $( < $($gen),* > )? {
            fn partial_cmp(&self, other: &Self) -> Option<::core::cmp::Ordering>{
                Some(self.cmp(other))
            }
        }
        impl$( <$($gen),* > )? Ord for $name $( < $($gen),* > )? {
            fn cmp(&self, other: &Self) -> ::core::cmp::Ordering{
                self.into_u32().cmp(&other.into_u32())
            }
        }

        impl$( <$($gen),* > )? ::std::hash::Hash for $name $( < $($gen),* > )? {
            fn hash<H: ::std::hash::Hasher>(&self, state: &mut H) {
                self.id.hash(state)
            }
        }
        impl$( <$($gen),* > )? ::std::fmt::Debug for $name $( < $($gen),* > )? {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                f.debug_struct(stringify!($name))
                    .field("id", &self.into_u32())
                    .finish()
            }
        }
        unsafe impl$( <$($gen),* > )? Send for $name $( < $($gen),* > )? {}
        unsafe impl$( <$($gen),* > )? Sync for $name $( < $($gen),* > )? {}

    };
}
