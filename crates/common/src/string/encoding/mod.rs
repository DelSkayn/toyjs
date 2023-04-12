use core::fmt;
use std::{iter::FusedIterator, ops::Index, slice::Iter};

mod ascii;
pub use ascii::{Ascii, AsciiChars};
mod utf16;
use hashbrown::Equivalent;
pub use utf16::{Utf16, Utf16Chars};

use super::String;

pub struct Utf16Error {
    valid_up_to: usize,
}

/// A enum with either ascii or utf16 code units
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Encoding<'a> {
    Ascii(&'a Ascii),
    Utf16(&'a Utf16),
}

impl<'a> Encoding<'a> {
    #[inline]
    pub fn slice<I>(self, index: I) -> Self
    where
        Ascii: Index<I, Output = Ascii>,
        Utf16: Index<I, Output = Utf16>,
    {
        match self {
            Self::Ascii(x) => Self::Ascii(&x[index]),
            Self::Utf16(x) => Self::Utf16(&x[index]),
        }
    }

    pub fn len(self) -> usize {
        match self {
            Self::Ascii(x) => x.len(),
            Self::Utf16(x) => x.len(),
        }
    }

    #[must_use]
    pub fn is_empty(self) -> bool {
        self.len() == 0
    }

    pub fn chars(self) -> Chars<'a> {
        match self {
            Self::Ascii(x) => Chars::Ascii(x.chars()),
            Self::Utf16(x) => Chars::Utf16(x.chars()),
        }
    }

    pub fn units(self) -> Units<'a> {
        match self {
            Self::Ascii(x) => Units::Ascii(x.units().iter()),
            Self::Utf16(x) => Units::Utf16(x.units().iter()),
        }
    }
}

impl<'a> Equivalent<String> for Encoding<'a> {
    fn equivalent(&self, key: &String) -> bool {
        key.encoding() == *self
    }
}

impl<'a> From<&'a Ascii> for Encoding<'a> {
    fn from(value: &'a Ascii) -> Self {
        Self::Ascii(value)
    }
}

impl<'a> From<&'a Utf16> for Encoding<'a> {
    fn from(value: &'a Utf16) -> Self {
        Self::Utf16(value)
    }
}

impl<'a> fmt::Display for Encoding<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Encoding::Ascii(ref x) => x.fmt(f),
            Encoding::Utf16(ref x) => x.fmt(f),
        }
    }
}

/// An iterator over either ascii or utf16 code units returning char's
pub enum Chars<'a> {
    Ascii(AsciiChars<'a>),
    Utf16(Utf16Chars<'a>),
}

impl Iterator for Chars<'_> {
    type Item = char;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match *self {
            Chars::Ascii(ref mut x) => x.next(),
            Chars::Utf16(ref mut x) => x.next(),
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        match *self {
            Chars::Ascii(ref x) => x.size_hint(),
            Chars::Utf16(ref x) => x.size_hint(),
        }
    }
}

impl FusedIterator for Chars<'_> {}

/// An iterator over either utf16 code units returning u16's
#[derive(Clone)]
pub enum Units<'a> {
    Ascii(Iter<'a, u8>),
    Utf16(Iter<'a, u16>),
}

impl Iterator for Units<'_> {
    type Item = u16;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match *self {
            Units::Ascii(ref mut x) => x.next().copied().map(|x| x.into()),
            Units::Utf16(ref mut x) => x.next().copied(),
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        match *self {
            Units::Ascii(ref x) => x.size_hint(),
            Units::Utf16(ref x) => x.size_hint(),
        }
    }
}

impl FusedIterator for Units<'_> {}
impl ExactSizeIterator for Units<'_> {
    #[inline]
    fn len(&self) -> usize {
        match *self {
            Units::Ascii(ref x) => x.len(),
            Units::Utf16(ref x) => x.len(),
        }
    }
}
