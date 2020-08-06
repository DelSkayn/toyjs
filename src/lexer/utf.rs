use super::*;

impl<'a> Lexer<'a> {
    #[inline(always)]
    pub fn is_non_ascii(val: u8) -> bool {
        val & 0x80 != 0
    }

    fn next_continue_byte(&mut self) -> Result<u8> {
        let byte = self
            .peek_byte()
            .ok_or_else(|| self.error(LexerErrorKind::InvalidUtf8))?;
        // A continueing byte must be in the form of 0b10xx_xxxx
        if byte & 0xC0 != 0x80 {
            return Err(self.error(LexerErrorKind::InvalidUtf8));
        }
        Ok(self.next_byte().unwrap())
    }

    /// Returns the next character from the stream if the character is
    /// a valid utf8 character and moves the stream past the character.
    pub fn next_char(&mut self) -> Result<Option<char>> {
        if let Some((x, idx)) = self.pending_char.take() {
            self.cur += idx as usize;
            return Ok(Some(x));
        }
        let start = if let Some(x) = self.peek_byte() {
            x
        } else {
            return Ok(None);
        };
        match (!start).leading_zeros() {
            0 => {
                self.next_byte();
                Ok(Some(start.into()))
            }
            2 => {
                // 0b110x_xxxx
                let mut val = 0u32;
                val |= (self.next_byte().unwrap() & 0b0001_1111) as u32;
                val <<= 6;
                let byte = self.next_continue_byte()?;
                val |= (byte & 0b0011_1111) as u32;
                char::from_u32(val)
                    .ok_or_else(|| self.error(LexerErrorKind::InvalidUtf8))
                    .map(Some)
            }
            3 => {
                // 0b1110_xxxx
                let mut val = 0u32;
                val |= (self.next_byte().unwrap() & 0b0000_1111) as u32;
                val <<= 6;
                let byte = self.next_continue_byte()?;
                val |= (byte & 0b0011_1111) as u32;
                val <<= 6;
                let byte = self.next_continue_byte()?;
                val |= (byte & 0b0011_1111) as u32;
                char::from_u32(val)
                    .ok_or_else(|| self.error(LexerErrorKind::InvalidUtf8))
                    .map(Some)
            }
            4 => {
                // 0b1111_0xxx
                let mut val = 0u32;
                val |= (self.next_byte().unwrap() & 0b0000_0111) as u32;
                val <<= 6;
                let byte = self.next_continue_byte()?;
                val |= (byte & 0b0011_1111) as u32;
                val <<= 6;
                let byte = self.next_continue_byte()?;
                val |= (byte & 0b0011_1111) as u32;
                val <<= 6;
                let byte = self.next_continue_byte()?;
                val |= (byte & 0b0011_1111) as u32;
                char::from_u32(val)
                    .ok_or_else(|| self.error(LexerErrorKind::InvalidUtf8))
                    .map(Some)
            }
            _ => Err(self.error(LexerErrorKind::InvalidUtf8)),
        }
    }

    /// Returns the next character from the stream if the character is
    /// a valid utf8 character
    pub fn peek_char(&mut self) -> Result<Option<char>> {
        if let Some((x, _)) = self.pending_char {
            return Ok(Some(x));
        };
        let cur = self.cur;
        let res = self.next_char()?;
        if let Some(x) = res {
            self.pending_char = Some((x, (self.cur - cur) as u8));
        };
        self.cur = cur;
        Ok(res)
    }
}
