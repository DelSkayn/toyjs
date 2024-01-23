use std::{
    fmt::{self, Write},
    ops::Index,
};

use super::Utf16Error;
use crate::{
    span::Span,
    unicode::{units, Utf16Ext},
};

/// A string of utf16 characters encoded as u16 guarenteed to be a valid utf16 sequence
#[derive(Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct Utf16([u16]);

impl Utf16 {
    /// Create utf16 slice, returns an error if the slice does not contain valid utf16.
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

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Create utf16 slice from a slice of u16 values.
    ///
    /// # Safety
    /// All values in the given slice must be valid utf16 and the slice must not start with a
    /// trailing surrogate or end with a leading surrogate.
    pub unsafe fn from_slice_unchecked(slice: &[u16]) -> &Self {
        // SAFETY: safe since Utf16 is repr transparent.
        std::mem::transmute(slice)
    }

    /// Returns a slice over the code units of the utf16 string.
    pub fn units(&self) -> &[u16] {
        // SAFETY: safe since Utf16 is repr transparent.
        unsafe { std::mem::transmute(self) }
    }

    /// Returns a iterator over the characters in this string.
    pub fn chars(&self) -> Utf16Chars {
        Utf16Chars(self)
    }

    /// Trim whitespace charaters at the start of the string
    #[inline]
    pub fn trim_start(&self) -> &Utf16 {
        if let Some((x, _)) = self
            .0
            .iter()
            .enumerate()
            .find(|(_, x)| !units::WHITE_SPACE.contains(x))
        {
            unsafe { Utf16::from_slice_unchecked(&self.0[x..]) }
        } else {
            self
        }
    }

    /// Trim whitespace charaters at the end of the string
    #[inline]
    pub fn trim_end(&self) -> &Utf16 {
        if let Some((x, _)) = self
            .0
            .iter()
            .enumerate()
            .rev()
            .find(|(_, x)| !units::WHITE_SPACE.contains(x))
        {
            unsafe { Utf16::from_slice_unchecked(&self.0[..=x]) }
        } else {
            self
        }
    }

    /// Trim whitespace charaters at the start and end of the charaters
    #[inline]
    pub fn trim(&self) -> &Utf16 {
        self.trim_start().trim_end()
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

impl DoubleEndedIterator for Utf16Chars<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let (first, rest) = self.0 .0.split_last()?;
        // Is it a surrogate
        if first.is_utf16_surrogate() {
            let (second, rest) = rest.split_last().expect("orphan utf16 surrogate in string");
            self.0 = unsafe { Utf16::from_slice_unchecked(rest) };

            let c = second.utf16_extend(*first);
            Some(unsafe { char::from_u32_unchecked(c) })
        } else {
            self.0 = unsafe { Utf16::from_slice_unchecked(rest) };
            Some(unsafe { char::from_u32_unchecked(*first as u32) })
        }
    }
}

impl<Idx> Index<Idx> for Utf16
where
    [u16]: Index<Idx, Output = [u16]>,
{
    type Output = Utf16;

    #[inline]
    fn index(&self, index: Idx) -> &Self::Output {
        //TODO: Possibly implement specialization over specific ranges for improved performance.
        let units = &self.units()[index];
        if units
            .first()
            .map(|x| x.is_utf16_trailing_surrogate())
            .unwrap_or(false)
        {
            panic!("invalid utf16 range, range start within utf16 surrogate pair");
        }
        if units
            .last()
            .map(|x| x.is_utf16_leading_surrogate())
            .unwrap_or(false)
        {
            panic!("invalid utf16 range, range start within utf16 surrogate pair");
        }

        unsafe { Self::from_slice_unchecked(units) }
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
