use std::{
    fmt::{self, Write},
    ops::{Index, Range},
};

use crate::{span::Span, unicode::Utf16Ext};

use super::Utf16Error;

/// A string of utf16 characters encoded as u16 guarenteed to be a valid utf16 sequence
#[derive(Eq, PartialEq)]
#[repr(transparent)]
pub struct Utf16([u16]);

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

    /// Returns the number of code units in the string.
    /// Can be more then the number of characters
    pub fn len(&self) -> usize {
        self.0.len()
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

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.0.len() / 2, Some(self.0.len()))
    }
}

impl Index<Range<usize>> for Utf16 {
    type Output = Utf16;

    #[inline]
    fn index(&self, index: Range<usize>) -> &Self::Output {
        let units = self.units();
        if units[index.start].is_utf16_trailing_surrogate() {
            panic!("invalid utf16 range, range start within utf16 surrogate pair");
        }
        if units[index.end - 1].is_utf16_leading_surrogate() {
            panic!("invalid utf16 range, range ends within utf16 surrogate pair");
        }
        unsafe { Self::from_slice_unchecked(&self.units()[index]) }
    }
}

impl Index<Span> for Utf16 {
    type Output = Utf16;

    #[inline]
    fn index(&self, index: Span) -> &Self::Output {
        let from = index.offset();
        let to = from + index.size();
        &self[from..to]
    }
}
