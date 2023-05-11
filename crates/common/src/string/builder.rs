use core::fmt;

use crate::unicode::{CharExt, Utf16Ext};

use super::{Ascii, Encoding, String, Utf16};

pub struct StringBuilder {
    pub ascii: Vec<u8>,
    pub utf16: Vec<u16>,
    is_ascii: bool,
}

impl Default for StringBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl StringBuilder {
    pub fn new() -> Self {
        Self {
            ascii: Vec::new(),
            utf16: Vec::new(),
            is_ascii: true,
        }
    }

    pub fn is_ascii(&self) -> bool {
        self.is_ascii
    }

    pub fn push(&mut self, unit: u16) {
        if self.is_ascii {
            if unit.is_ascii() {
                self.ascii.push(unit as u8);
            } else {
                self.utf16.reserve(self.ascii.len());
                self.ascii.drain(..).for_each(|x| self.utf16.push(x as u16));
                self.is_ascii = false;
                self.utf16.push(unit);
            }
        } else {
            self.utf16.push(unit);
        }
    }

    pub fn take(&mut self) -> String {
        if self.is_ascii {
            let ascii = unsafe { Ascii::from_slice_unchecked(&self.ascii) };
            let res = String::from(ascii);
            self.ascii.clear();
            res
        } else {
            let ascii = unsafe { Utf16::from_slice_unchecked(&self.utf16) };
            let res = String::from(ascii);
            self.utf16.clear();
            self.is_ascii = true;
            res
        }
    }

    // TODO: Since access to the buffers is allowed, this function should be unsafe.
    pub fn encoding(&self) -> Encoding {
        unsafe {
            if self.is_ascii {
                Encoding::Ascii(Ascii::from_slice_unchecked(&self.ascii))
            } else {
                Encoding::Utf16(Utf16::from_slice_unchecked(&self.utf16))
            }
        }
    }

    pub fn clear(&mut self) {
        if self.is_ascii {
            self.ascii.clear()
        } else {
            self.is_ascii = true;
            self.utf16.clear()
        }
    }
}

impl fmt::Write for StringBuilder {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for c in s.chars() {
            self.write_char(c)?;
        }

        Ok(())
    }

    fn write_char(&mut self, c: char) -> fmt::Result {
        let (first, rest) = c.encode_utf16_code_point();
        self.push(first);
        if let Some(rest) = rest {
            self.push(rest);
        }
        Ok(())
    }
}
