use super::*;
use crate::token::NumberKind;
use std::str;

impl<'a> Lexer<'a> {
    fn token_num(&mut self, num: NumberKind) -> Result<Option<Token>> {
        self.token(TokenKind::Lit(LitToken::Number(num)))
    }

    #[inline]
    pub fn is_digit(c: u8) -> bool {
        c >= b'0' && c <= b'9'
    }

    #[inline]
    pub fn is_radix(c: u8, radix: u8) -> bool {
        if c >= b'0' && c < (b'9' + 1).min(b'0' + radix) {
            return true;
        }
        if c >= b'a' && c < b'a' + radix - 10 {
            return true;
        }
        if c >= b'A' && c < b'A' + radix - 10 {
            return true;
        }
        return false;
    }

    pub fn from_digit(c: u8) -> u8 {
        if c >= b'0' && c <= b'9' {
            return c - b'0';
        }
        if c >= b'a' && c <= b'f' {
            return c - b'a' + 1;
        }
        if c >= b'A' && c <= b'F' {
            return c - b'A' + 1;
        }
        panic!("invalid digit");
    }

    pub fn lex_number(&mut self, start: u8) -> Result<Option<Token>> {
        let str_start = self.cur - 1;
        if start == b'0' {
            match self.peek_byte() {
                None => return self.token_num(NumberKind::Integer(0)),
                Some(b'n') => {
                    self.next_byte();
                    let s = self.interner.intern("0");
                    return self.token_num(NumberKind::Big(s));
                }
                Some(b'b') | Some(b'B') => {
                    self.next_byte();
                    return self.lex_non_decimal(2, str_start);
                }
                Some(b'o') | Some(b'O') => {
                    self.next_byte();
                    return self.lex_non_decimal(8, str_start);
                }
                Some(b'x') | Some(b'X') => {
                    self.next_byte();
                    return self.lex_non_decimal(16, str_start);
                }
                _ => {}
            }
        }
        if start == b'.' {
            return self.lex_number_mantissa(str_start);
        }
        while self.peek_byte().map(|x| Self::is_digit(x)).unwrap_or(false) {
            self.next_byte();
        }
        let mut big = false;
        match self.peek_byte() {
            Some(b'.') => {
                self.next_byte();
                return self.lex_number_mantissa(str_start);
            }
            Some(b'e') | Some(b'E') => {
                self.next_byte();
                return self.lex_number_exponent(str_start);
            }
            Some(b'n') => {
                self.next_byte();
                big = true;
            }
            _ => (),
        }
        if big {
            let s = str::from_utf8(&self.bytes[str_start..self.cur]).unwrap();
            let s = self.interner.intern(s);
            self.token_num(NumberKind::Big(s))
        } else {
            self.lex_number_string(str_start)
        }
    }

    fn lex_number_string(&mut self, str_start: usize) -> Result<Option<Token>> {
        let num: f64 = lexical_core::parse(&self.bytes[str_start..self.cur]).unwrap();
        if (num as i32) as f64 == num {
            self.token_num(NumberKind::Integer(num as i32))
        } else {
            self.token_num(NumberKind::Float(num))
        }
    }

    fn lex_number_mantissa(&mut self, str_start: usize) -> Result<Option<Token>> {
        while self.peek_byte().map(Self::is_digit).unwrap_or(false) {
            self.next_byte();
        }
        match self.peek_byte() {
            Some(b'e') | Some(b'E') => {
                self.next_byte();
                return self.lex_number_exponent(str_start);
            }
            _ => {}
        }
        self.lex_number_string(str_start)
    }

    fn lex_number_exponent(&mut self, str_start: usize) -> Result<Option<Token>> {
        let start = self.peek_byte();
        if start.is_none() {
            return Err(self.error(LexerErrorKind::InvalidNumber));
        }
        let start = start.unwrap();
        if start == b'-' || start == b'+' || !Self::is_digit(start) {
            self.next_byte();
            while self.peek_byte().map(Self::is_digit).unwrap_or(false) {
                self.next_byte();
            }
            return self.lex_number_string(str_start);
        }
        Err(self.error(LexerErrorKind::InvalidNumber))
    }

    pub fn lex_non_decimal(&mut self, radix: u8, str_start: usize) -> Result<Option<Token>> {
        while self
            .peek_byte()
            .map(|c| Self::is_radix(c, radix))
            .unwrap_or(false)
        {
            self.next_byte();
        }
        let mut big = false;
        if let Some(b'n') = self.peek_byte() {
            self.next_byte();
            big = true;
        }
        let s = str::from_utf8(&self.bytes[str_start + 2..self.cur]).unwrap();
        if big {
            let s = self.interner.intern(s);
            self.token_num(NumberKind::Big(s))
        } else {
            //TODO dont think this is correct way of parsing.
            let num = u64::from_str_radix(s, radix as u32)
                .map_err(|_| self.error(LexerErrorKind::InvalidNumber))?;
            if num as i32 as u64 == num {
                self.token_num(NumberKind::Integer(num as i32))
            } else {
                self.token_num(NumberKind::Float(num as f64))
            }
        }
    }
}
