//! handling characters which are not ASCII.

use super::*;
use std::char;
use unicode_xid::UnicodeXID;

const CONTINUE_BYTE_MASK: u8 = 0b0011_1111;

impl<'a> Lexer<'a> {
    /// Returns the next byte which has a utf-8 continue marker.
    fn next_continue_byte(&mut self) -> LexResult<u8> {
        let peek = self.peek_byte().ok_or(ErrorKind::InvalidUnicodeSequence)?;
        if peek & 0xC0 != 0x80 {
            Err(ErrorKind::InvalidUnicodeSequence)
        } else {
            self.eat_byte();
            Ok(peek)
        }
    }

    /// Returns the next character form the byte stream.
    pub(super) fn next_char(&mut self, next: u8) -> LexResult<char> {
        match (!next).leading_zeros() {
            0 => Ok(char::from(next)),
            2 => {
                let mut val = (next & 0b0001_1111) as u32;
                val <<= 6;
                val |= (self.next_continue_byte()? & 0b00011_1111) as u32;
                char::from_u32(val).ok_or(ErrorKind::InvalidUnicodeSequence)
            }
            3 => {
                let mut val = (next & 0b0000_1111) as u32;
                val <<= 6;
                val |= (self.next_continue_byte()? & CONTINUE_BYTE_MASK) as u32;
                val <<= 6;
                val |= (self.next_continue_byte()? & CONTINUE_BYTE_MASK) as u32;
                char::from_u32(val).ok_or(ErrorKind::InvalidUnicodeSequence)
            }
            4 => {
                let mut val = (next & 0b0000_0111) as u32;
                val <<= 6;
                val |= (self.next_continue_byte()? & CONTINUE_BYTE_MASK) as u32;
                val <<= 6;
                val |= (self.next_continue_byte()? & CONTINUE_BYTE_MASK) as u32;
                val <<= 6;
                val |= (self.next_continue_byte()? & CONTINUE_BYTE_MASK) as u32;
                char::from_u32(val).ok_or(ErrorKind::InvalidUnicodeSequence)
            }
            _ => Err(ErrorKind::InvalidUnicodeSequence),
        }
    }

    /// Peeks the next full character.
    pub(super) fn peek_char(&mut self, next: u8) -> LexResult<char> {
        // Kinda cheating but it works for now.
        // TODO look into a better way to do this
        let offset = self.offset;
        let res = self.next_char(next);
        self.offset = offset;
        res
    }

    /// Parses a non-ascii character.
    ///
    /// next should be the first byte of the non-ascii character.
    ///
    /// Non-ascii tokens can only be identifiers, whitespace or line terminators.
    pub(super) fn match_char(&mut self, next: u8) -> LexResult<Option<Token>> {
        // The start of a possible identifier is at the previous byte.
        let start = self.offset - 1;
        let res = match self.next_char(next)? {
            // Line terminator
            chars::LS | chars::PS | chars::BS => self.token(t!("\n")),
            // White space
            chars::ZWJ | chars::ZWNBSP | chars::ZWNJ => {
                self.lex_whitespace()?;
                return self.next_inner();
            }
            // Is a normal identifier.
            x if x.is_xid_start() => self.lex_ident(start)?,
            _ => return Err(ErrorKind::InvalidToken),
        };

        Ok(Some(res))
    }
}
