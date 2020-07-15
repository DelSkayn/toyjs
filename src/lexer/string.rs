use super::*;
use std::convert::TryInto;

impl<'a> Lexer<'a> {
    pub fn lex_string(&mut self, start: u8) -> Result<Option<Token>> {
        self.buffer.clear();
        loop {
            match self
                .eat()
                .ok_or_else(|| self.error(LexerErrorKind::UnClosedString))?
            {
                b'\\' => self.lex_escape_code()?,
                chars::LF | chars::CR => return Err(self.error(LexerErrorKind::UnClosedString)),
                s if s == start => {
                    let s = self.interner.intern(&self.buffer);
                    return self.token(TokenKind::Lit(LitToken::String(s)));
                }
                x if Self::is_non_ascii(x) => match self.eat_char()?.unwrap() {
                    chars::LS | chars::PS => return Err(self.error(LexerErrorKind::UnClosedString)),
                    x => self.buffer.push(x),
                },
                x => self.buffer.push(x.into()),
            }
        }
    }

    fn digit_from_byte(&mut self, c: u8) -> Result<u8> {
        if !Self::is_radix(c, 16) {
            return Err(self.error(LexerErrorKind::InvalidEscapeCode));
        }
        Ok(Self::from_digit(c))
    }

    fn lex_escape_code(&mut self) -> Result<()> {
        let next = if let Some(x) = self.eat() {
            x
        } else {
            return Err(self.error(LexerErrorKind::InvalidUtf8));
        };
        match next {
            b'\\' => self.buffer.push('\\'),
            b'\'' => self.buffer.push('\''),
            b'\"' => self.buffer.push('\"'),
            b'0' => self.buffer.push('\0'),
            b'b' => self.buffer.push(chars::BS.into()),
            b't' => self.buffer.push(chars::HT.into()),
            b'f' => self.buffer.push(chars::FF.into()),
            b'n' => self.buffer.push(chars::LF.into()),
            b'v' => self.buffer.push(chars::VT.into()),
            b'r' => self.buffer.push(chars::CR.into()),
            chars::LF => {
                if self.peek() == Some(chars::CR) {
                    self.eat();
                }
            }
            chars::CR => {}
            b'x' => {
                let mut val = 0u8;
                for _ in 0..2 {
                    match self.eat() {
                        Some(e) => {
                            let digit = self.digit_from_byte(e)?;
                            val <<= 4;
                            val |= digit;
                        }
                        None => return Err(self.error(LexerErrorKind::InvalidEscapeCode)),
                    }
                }
                self.buffer.push(val.into());
            }
            b'u' => {
                let next = if let Some(x) = self.eat() {
                    x
                } else {
                    return Err(self.error(LexerErrorKind::InvalidEscapeCode));
                };
                if next == b'{' {
                    let mut val = 0u32;
                    let mut finished = false;
                    for i in 0..6 {
                        match self.eat() {
                            Some(b'}') => {
                                if i == 0 {
                                    return Err(self.error(LexerErrorKind::InvalidEscapeCode));
                                }
                                finished = true;
                                break;
                            }
                            Some(x) if Self::is_radix(x, 16) => {
                                val <<= 4;
                                val |= Self::from_digit(x) as u32;
                            }
                            _ => return Err(self.error(LexerErrorKind::InvalidEscapeCode)),
                        }
                    }
                    if !finished {
                        if self.eat() != Some(b'}') {
                            return Err(self.error(LexerErrorKind::InvalidEscapeCode));
                        }
                    }
                    let val: char = val
                        .try_into()
                        .map_err(|_| self.error(LexerErrorKind::InvalidEscapeCode))?;
                    self.buffer.push(val)
                } else {
                    let mut val = 0u32;
                    for _ in 0..4 {
                        match self.eat() {
                            Some(e) => {
                                let digit = self.digit_from_byte(e)?;
                                val <<= 4;
                                val |= digit as u32;
                            }
                            None => return Err(self.error(LexerErrorKind::InvalidEscapeCode)),
                        }
                    }
                    let val: char = val
                        .try_into()
                        .map_err(|_| self.error(LexerErrorKind::InvalidEscapeCode))?;
                    self.buffer.push(val)
                }
            }
            x if Self::is_non_ascii(x) => match self.eat_char()? {
                Some(chars::LS) | Some(chars::PS) => {}
                _ => return Err(self.error(LexerErrorKind::InvalidEscapeCode)),
            },
            _ => return Err(self.error(LexerErrorKind::InvalidEscapeCode)),
        };
        Ok(())
    }
}
