use std::fmt::{self, Write};

use crate::unicode::Utf16Ext;

/// A string of utf16 characters encoded as u16 guarenteed to be a valid utf16 sequence
#[derive(Eq, PartialEq)]
#[repr(transparent)]
pub struct Utf16([u16]);

pub struct Utf16Error {
    valid_up_to: usize,
}

impl Utf16 {
    pub fn from_slice(slice: &[u16]) -> Result<&Self, Utf16Error> {
        let mut iter = slice.iter().enumerate();

        while let Some((idx, x)) = iter.next() {
            if x.is_utf16_leading_surrogate()
                && iter
                    .next()
                    .map(|(_, x)| x.is_utf16_trailing_surrogate())
                    .unwrap_or(false)
            {
                return Err(Utf16Error { valid_up_to: idx });
            }

            if x.is_utf16_trailing_surrogate() {
                return Err(Utf16Error { valid_up_to: idx });
            }
        }

        unsafe { Ok(Self::from_slice_unchecked(slice)) }
    }

    /// Create utf16 slice from a slice of u16 values.
    ///
    /// # Safety
    /// All values in the given slice must be valid utf16 and the slice must not start with a
    /// trailing surrogate.
    pub unsafe fn from_slice_unchecked(slice: &[u16]) -> &Self {
        // SAFETY: safe since Utf16 is repr transparent.
        std::mem::transmute(slice)
    }

    pub fn units(&self) -> &[u16] {
        // SAFETY: safe since Utf16 is repr transparent.
        unsafe { std::mem::transmute(self) }
    }

    pub fn chars(&self) -> Utf16Chars {
        Utf16Chars(self)
    }
}

impl fmt::Display for Utf16 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.chars().try_for_each(|x| f.write_char(x))
    }
}

impl fmt::Debug for Utf16 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char('"')?;
        self.chars().try_for_each(|x| write!(f, "{:?}", x))?;
        f.write_char('"')
    }
}

/// An iterator over utf16 code units returning chars
pub struct Utf16Chars<'a>(&'a Utf16);

impl Iterator for Utf16Chars<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let (first, rest) = self.0 .0.split_first()?;
        // Is it a surrogate
        if first.is_utf16_surrogate() {
            let (second, rest) = rest
                .split_first()
                .expect("orphan utf16 surrogate in string");
            self.0 = unsafe { Utf16::from_slice_unchecked(rest) };

            let c = first.utf16_extend(*second);
            Some(unsafe { char::from_u32_unchecked(c) })
        } else {
            self.0 = unsafe { Utf16::from_slice_unchecked(rest) };
            Some(unsafe { char::from_u32_unchecked(*first as u32) })
        }
    }
}

/// A string of ascii characters encoded as u8.
#[derive(Eq, PartialEq)]
#[repr(transparent)]
pub struct Ascii([u8]);

impl Ascii {
    pub fn from_slice(slice: &[u8]) -> Result<&Self, Utf16Error> {
        if let Some(x) = slice.iter().enumerate().find(|x| !x.1.is_ascii()) {
            Err(Utf16Error { valid_up_to: x.0 })
        } else {
            unsafe { Ok(Self::from_slice_unchecked(slice)) }
        }
    }

    /// # Safety
    /// slice must contain only valid ascii characters
    pub unsafe fn from_slice_unchecked(slice: &[u8]) -> &Self {
        // SAFETY: safe since ascii is repr transparent.
        std::mem::transmute(slice)
    }

    pub fn units(&self) -> &[u8] {
        // SAFETY: safe since ascii is repr transparent.
        unsafe { std::mem::transmute(self) }
    }

    pub fn chars(&self) -> AsciiChars {
        AsciiChars(self)
    }

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

    fn next(&mut self) -> Option<Self::Item> {
        let (first, rest) = self.0 .0.split_first()?;
        self.0 = unsafe { Ascii::from_slice_unchecked(rest) };
        Some(unsafe { char::from_u32_unchecked(*first as u32) })
    }
}

/// A enum with either ascii or utf16 code units
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Encoding<'a> {
    Ascii(&'a Ascii),
    Utf16(&'a Utf16),
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

    fn next(&mut self) -> Option<Self::Item> {
        match *self {
            Chars::Ascii(ref mut x) => x.next(),
            Chars::Utf16(ref mut x) => x.next(),
        }
    }
}
