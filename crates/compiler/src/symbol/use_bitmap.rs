use core::fmt;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, Not};

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct UseBitmap(u128);

impl fmt::Display for UseBitmap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#0128b}", self.0)
    }
}

impl fmt::Debug for UseBitmap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(self, f)
    }
}

impl Not for UseBitmap {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self(!self.0)
    }
}

impl BitOr for UseBitmap {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        UseBitmap(self.0 | rhs.0)
    }
}

impl BitOrAssign for UseBitmap {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0
    }
}

impl BitAnd for UseBitmap {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        UseBitmap(self.0 & rhs.0)
    }
}

impl BitAndAssign for UseBitmap {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0
    }
}

impl UseBitmap {
    pub fn empty() -> Self {
        UseBitmap(0)
    }

    pub fn get(&self, bit: u8) -> bool {
        (self.0 & 1 << bit) != 0
    }

    pub fn set(&mut self, bit: u8) {
        self.0 |= 1 << bit
    }

    pub fn unset(&mut self, bit: u8) {
        self.0 &= !(1 << bit)
    }

    pub fn next_free(&self) -> u8 {
        self.0.trailing_ones() as u8
    }
}
