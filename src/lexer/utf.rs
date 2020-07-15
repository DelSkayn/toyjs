use super::*;

impl<'a> Lexer<'a> {
    #[inline(always)]
    pub fn is_non_ascii(val: u8) -> bool {
        val & 0x80 != 0
    }

    fn eat_continue_byte(&mut self) -> Result<u8> {
        let byte = self
            .peek()
            .ok_or_else(|| self.error(LexerErrorKind::InvalidUtf8))?;
        // A continueing byte must be in the form of 0b10xx_xxxx
        if byte & 0xC0 != 0x80 {
            return Err(self.error(LexerErrorKind::InvalidUtf8));
        }
        Ok(self.eat().unwrap())
    }

    pub fn eat_char(&mut self) -> Result<Option<char>> {
        if let Some((x, idx)) = self.peek.take() {
            self.cur += idx as usize;
            return Ok(Some(x));
        }
        let start = if let Some(x) = self.peek() {
            x
        } else {
            return Ok(None);
        };
        match (!start).leading_zeros() {
            0 => {
                self.eat();
                Ok(Some(start.into()))
            }
            2 => {
                // 0b110x_xxxx
                let mut val = 0u32;
                val |= (self.eat().unwrap() & 0b0001_1111) as u32;
                val <<= 6;
                let byte = self.eat_continue_byte()?;
                val |= (byte & 0b0011_1111) as u32;
                char::from_u32(val)
                    .ok_or_else(|| self.error(LexerErrorKind::InvalidUtf8))
                    .map(Some)
            }
            3 => {
                // 0b1110_xxxx
                let mut val = 0u32;
                val |= (self.eat().unwrap() & 0b0000_1111) as u32;
                val <<= 6;
                let byte = self.eat_continue_byte()?;
                val |= (byte & 0b0011_1111) as u32;
                val <<= 6;
                let byte = self.eat_continue_byte()?;
                val |= (byte & 0b0011_1111) as u32;
                char::from_u32(val)
                    .ok_or_else(|| self.error(LexerErrorKind::InvalidUtf8))
                    .map(Some)
            }
            4 => {
                // 0b1111_0xxx
                let mut val = 0u32;
                val |= (self.eat().unwrap() & 0b0000_0111) as u32;
                val <<= 6;
                let byte = self.eat_continue_byte()?;
                val |= (byte & 0b0011_1111) as u32;
                val <<= 6;
                let byte = self.eat_continue_byte()?;
                val |= (byte & 0b0011_1111) as u32;
                val <<= 6;
                let byte = self.eat_continue_byte()?;
                val |= (byte & 0b0011_1111) as u32;
                char::from_u32(val)
                    .ok_or_else(|| self.error(LexerErrorKind::InvalidUtf8))
                    .map(Some)
            }
            _ => Err(self.error(LexerErrorKind::InvalidUtf8)),
        }
    }

    pub fn peek_char(&mut self) -> Result<Option<char>> {
        if let Some((x, _)) = self.peek {
            return Ok(Some(x));
        };
        let cur = self.cur;
        let res = self.eat_char()?;
        if let Some(x) = res {
            self.peek = Some((x, (self.cur - cur) as u32));
        };
        self.cur = cur;
        Ok(res)
    }
}
