use std::fmt::{self, Write};

use crate::unicode::Utf16Ext;

/// A string of utf16 characters encoded as u16 guarenteed to be a valid utf16 sequence
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Utf16<'a>(&'a [u16]);

pub struct Utf16Error {
    valid_up_to: usize,
}

impl<'a> Utf16<'a> {
    pub fn from_slice(slice: &'a [u16]) -> Result<Self, Utf16Error> {
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
    pub unsafe fn from_slice_unchecked(slice: &'a [u16]) -> Self {
        Self(slice)
    }

    pub fn units(self) -> &'a [u16] {
        self.0
    }

    pub fn chars(self) -> Utf16Chars<'a> {
        Utf16Chars(self)
    }
}

impl<'a> fmt::Display for Utf16<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.chars().try_for_each(|x| f.write_char(x))
    }
}

impl<'a> fmt::Debug for Utf16<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char('"')?;
        self.chars().try_for_each(|x| write!(f, "{:?}", x))?;
        f.write_char('"')
    }
}

/// An iterator over utf16 code units returning chars
pub struct Utf16Chars<'a>(Utf16<'a>);

impl Iterator for Utf16Chars<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let (first, rest) = self.0 .0.split_first()?;
        // Is it a surrogate
        if first.is_utf16_surrogate() {
            let (second, rest) = rest
                .split_first()
                .expect("orphan utf16 surrogate in string");
            self.0 .0 = rest;

            let c = first.is_utf16_extend(*second);
            Some(unsafe { char::from_u32_unchecked(c) })
        } else {
            self.0 .0 = rest;
            Some(unsafe { char::from_u32_unchecked(*first as u32) })
        }
    }
}

/// A string of ascii characters encoded as u8.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Ascii<'a>(&'a [u8]);

impl<'a> Ascii<'a> {
    pub fn from_slice(slice: &'a [u8]) -> Result<Self, Utf16Error> {
        if let Some(x) = slice.iter().enumerate().find(|x| !x.1.is_ascii()) {
            Err(Utf16Error { valid_up_to: x.0 })
        } else {
            unsafe { Ok(Self::from_slice_unchecked(slice)) }
        }
    }

    /// # Safety
    /// slice must contain only valid ascii characters
    pub unsafe fn from_slice_unchecked(slice: &'a [u8]) -> Self {
        Self(slice)
    }

    pub fn units(self) -> &'a [u8] {
        self.0
    }

    pub fn chars(self) -> AsciiChars<'a> {
        AsciiChars(self)
    }

    pub fn as_str(self) -> &'a str {
        // SAFETY: an ascii string is also a valid utf8 string
        unsafe { std::str::from_utf8_unchecked(self.0) }
    }
}

impl<'a> fmt::Display for Ascii<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl<'a> fmt::Debug for Ascii<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

/// An iterator over ascii code units returning chars
pub struct AsciiChars<'a>(Ascii<'a>);

impl Iterator for AsciiChars<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let (first, rest) = self.0 .0.split_first()?;
        self.0 .0 = rest;
        Some(unsafe { char::from_u32_unchecked(*first as u32) })
    }
}

/// A enum with either ascii or utf16 code units
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CodeUnits<'a> {
    Ascii(Ascii<'a>),
    Utf16(Utf16<'a>),
}

impl<'a> From<Ascii<'a>> for CodeUnits<'a> {
    fn from(value: Ascii<'a>) -> Self {
        Self::Ascii(value)
    }
}

impl<'a> From<Utf16<'a>> for CodeUnits<'a> {
    fn from(value: Utf16<'a>) -> Self {
        Self::Utf16(value)
    }
}

impl<'a> fmt::Display for CodeUnits<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            CodeUnits::Ascii(ref x) => x.fmt(f),
            CodeUnits::Utf16(ref x) => x.fmt(f),
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
