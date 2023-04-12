use core::fmt;
use std::ops::{Index, Range};

use crate::span::Span;

use super::Utf16Error;

/// A string of ascii characters encoded as u8.
#[derive(Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct Ascii([u8]);

impl Ascii {
    #[inline]
    pub fn from_slice(slice: &[u8]) -> Result<&Self, Utf16Error> {
        if let Some(x) = slice.iter().enumerate().find(|x| !x.1.is_ascii()) {
            Err(Utf16Error { valid_up_to: x.0 })
        } else {
            unsafe { Ok(Self::from_slice_unchecked(slice)) }
        }
    }

    pub const fn const_from_slice(slice: &[u8]) -> &Self {
        let mut i = 0;
        while i < slice.len() {
            if !slice[i].is_ascii() {
                panic!("byte slice was not utf8")
            }
            i += 1;
        }
        unsafe { Self::from_slice_unchecked(slice) }
    }

    pub const fn const_from_str(slice: &str) -> &Self {
        Self::const_from_slice(slice.as_bytes())
    }

    /// Returns the number of code units in the string.
    /// Equivalent to the number of characters for ascii.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// # Safety
    /// slice must contain only valid ascii characters
    #[inline]
    pub const unsafe fn from_slice_unchecked(slice: &[u8]) -> &Self {
        // SAFETY: safe since ascii is repr transparent.
        std::mem::transmute(slice)
    }

    #[inline]
    pub fn units(&self) -> &[u8] {
        // SAFETY: safe since ascii is repr transparent.
        unsafe { std::mem::transmute(self) }
    }

    /// Returns a iterator over the characters in this string.
    #[inline]
    pub fn chars(&self) -> AsciiChars {
        AsciiChars(self)
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        // SAFETY: an ascii string is also a valid utf8 string
        unsafe { std::str::from_utf8_unchecked(self.units()) }
    }
}

impl fmt::Display for Ascii {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl fmt::Debug for Ascii {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

/// An iterator over ascii code units returning chars
pub struct AsciiChars<'a>(&'a Ascii);

impl Iterator for AsciiChars<'_> {
    type Item = char;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let (first, rest) = self.0 .0.split_first()?;
        self.0 = unsafe { Ascii::from_slice_unchecked(rest) };
        Some(unsafe { char::from_u32_unchecked(*first as u32) })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.0.len(), Some(self.0.len()))
    }
}

impl Index<Range<usize>> for Ascii {
    type Output = Ascii;

    #[inline]
    fn index(&self, index: Range<usize>) -> &Self::Output {
        unsafe { Self::from_slice_unchecked(&self.units()[index]) }
    }
}

impl Index<Span> for Ascii {
    type Output = Ascii;

    #[inline]
    fn index(&self, index: Span) -> &Self::Output {
        let from = index.offset();
        let to = from + index.size();
        &self[from..to]
    }
}
