//! Module implementing number wrapper for floating point number which implement bitwise equality
//! and hashing

pub trait Num: Sized + Copy {
    fn equal_bits(self, other: Self) -> bool;

    fn cast_from_f64(num: f64) -> Self;

    fn cast_to_f64(self) -> f64;
}

macro_rules! impl_num{
    ($($n:ident),*) => {
        $(
            impl Num for $n {
                fn equal_bits(self,other: Self) -> bool{
                    let this: [u8; std::mem::size_of::<Self>()] = unsafe{
                        #[allow(clippy::transmute_num_to_bytes)]
                        std::mem::transmute(self)
                    };
                    let other: [u8; std::mem::size_of::<Self>()] = unsafe{
                        #[allow(clippy::transmute_num_to_bytes)]
                        std::mem::transmute(other)
                    };
                    this == other
                }

                fn cast_from_f64(num: f64) -> Self{
                    num as $n
                }

                fn cast_to_f64(self) -> f64{
                    self as f64
                }
            }
        )*
    }
}

impl_num!(u8, i8, u16, i16, u32, i32, u64, i64, f32, f64);

use std::hash::{Hash, Hasher};

use crate::key;

key!(pub struct NumberId(u32));

/// A wrapper around f64 which implements bitwise equility and hashing.
#[derive(Clone, Copy, Debug)]
pub struct Number(pub f64);

impl Number {
    /// Returns if the underlying number can be cast to a different number without changing its
    /// semantic value.
    pub fn fits<N: Num>(self) -> bool {
        N::cast_from_f64(self.0).cast_to_f64().equal_bits(self.0)
    }

    /// Cast the number to a different number if the current number can be represented in the given
    /// format without changing its semantic value.
    pub fn cast<N: Num>(self) -> Option<N> {
        if self.fits::<N>() {
            Some(N::cast_from_f64(self.0))
        } else {
            None
        }
    }
}

impl From<&Number> for Number {
    fn from(value: &Number) -> Self {
        Number(value.0)
    }
}

impl Hash for Number {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state)
    }
}
impl Eq for Number {}
impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_bits() == other.0.to_bits()
    }
}
