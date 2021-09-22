use super::*;
use std::convert::TryInto;
use token::{Literal, TokenKind};

fn from_ascii_digit(digit: u8) -> u8 {
    if digit.is_ascii_digit() {
        return digit - b'0';
    }
    if digit >= b'a' && digit <= b'f' {
        return digit - b'a' + 0xa;
    }
    if digit >= b'A' && digit <= b'F' {
        return digit - b'A' + 0xa;
    }
    panic!("invalid digit");
}

impl<'a> Lexer<'a> {
    /// Lex a string token.
    pub(super) fn lex_string(&mut self, start: u8) -> LexResult<Token> {
        self.buffer.clear();
        loop {
            match self.next_byte().ok_or(ErrorKind::UnClosedString)? {
                b'\\' => self.lex_escape_code()?,
                chars::LF | chars::CR => return Err(ErrorKind::UnClosedString),
                s if s == start => {
                    let s = self.interner.intern(&self.buffer);
                    return Ok(self.token(TokenKind::Literal(Literal::String(s))));
                }
                x if !x.is_ascii() => match self.next_char(x)? {
                    chars::LS | chars::PS => return Err(ErrorKind::UnClosedString),
                    x => self.buffer.push(x),
                },
                x => self.buffer.push(x.into()),
            }
        }
    }

    fn digit_from_byte(&mut self, c: u8) -> LexResult<u8> {
        if !is_radix(c, 16) {
            return Err(ErrorKind::InvalidEscapeCode);
        }
        Ok(from_ascii_digit(c))
    }

    /// Lexes escape codes in a string.
    fn lex_escape_code(&mut self) -> LexResult<()> {
        let next = if let Some(x) = self.next_byte() {
            x
        } else {
            return Err(ErrorKind::InvalidUnicodeSequence);
        };
        match next {
            b'\\' => self.buffer.push('\\'),
            b'\'' => self.buffer.push('\''),
            b'\"' => self.buffer.push('\"'),
            b'0' => self.buffer.push('\0'),
            b'b' => self.buffer.push(chars::BS),
            b't' => self.buffer.push(chars::HT.into()),
            b'f' => self.buffer.push(chars::FF.into()),
            b'n' => self.buffer.push(chars::LF.into()),
            b'v' => self.buffer.push(chars::VT.into()),
            b'r' => self.buffer.push(chars::CR.into()),
            chars::LF => {
                if self.peek_byte() == Some(chars::CR) {
                    self.next_byte();
                }
            }
            chars::CR => {}
            b'x' => {
                // a unicode escape sequence
                let mut val = 0u8;
                for _ in 0..2 {
                    match self.next_byte() {
                        Some(e) => {
                            let digit = self.digit_from_byte(e)?;
                            val <<= 4;
                            val |= digit;
                        }
                        None => return Err(ErrorKind::InvalidEscapeCode),
                    }
                }
                self.buffer.push(val.into());
            }
            b'u' => {
                // a unicode escape sequence
                let next = if let Some(x) = self.next_byte() {
                    x
                } else {
                    return Err(ErrorKind::InvalidEscapeCode);
                };
                if next == b'{' {
                    let mut val = 0u32;
                    let mut finished = false;
                    for i in 0..6 {
                        match self.next_byte() {
                            Some(b'}') => {
                                if i == 0 {
                                    return Err(ErrorKind::InvalidEscapeCode);
                                }
                                finished = true;
                                break;
                            }
                            Some(x) if is_radix(x, 16) => {
                                val <<= 4;
                                val |= from_ascii_digit(x) as u32;
                            }
                            _ => return Err(ErrorKind::InvalidEscapeCode),
                        }
                    }
                    if !finished && self.next_byte() != Some(b'}') {
                        return Err(ErrorKind::InvalidEscapeCode);
                    }
                    let val: char = val.try_into().map_err(|_| ErrorKind::InvalidEscapeCode)?;
                    self.buffer.push(val)
                } else {
                    let mut val = 0u32;
                    for _ in 0..4 {
                        match self.next_byte() {
                            Some(e) => {
                                let digit = self.digit_from_byte(e)?;
                                val <<= 4;
                                val |= digit as u32;
                            }
                            None => return Err(ErrorKind::InvalidEscapeCode),
                        }
                    }
                    let val: char = val.try_into().map_err(|_| ErrorKind::InvalidEscapeCode)?;
                    self.buffer.push(val)
                }
            }
            x if !x.is_ascii() => match self.next_char(x)? {
                chars::LS | chars::PS => {}
                _ => return Err(ErrorKind::InvalidEscapeCode),
            },
            _ => return Err(ErrorKind::InvalidEscapeCode),
        };
        Ok(())
    }
}
