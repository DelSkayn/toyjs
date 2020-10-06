use super::*;
use std::str;
use token::{Literal, Number, TokenKind};

impl<'a> Lexer<'a> {
    fn token_num(&mut self, kind: Number) -> Token {
        self.token(TokenKind::Literal(Literal::Number(kind)))
    }

    /// Lex a number
    pub(super) fn lex_number(&mut self, start: u8) -> LexResult<Token> {
        let str_start = self.offset - 1;
        if start == b'0' {
            match self.peek_byte() {
                None => return Ok(self.token_num(Number::Integer(0))),
                Some(b'n') => {
                    self.eat_byte();
                    let s = self.interner.intern("0");
                    return Ok(self.token_num(Number::Big(s)));
                }
                Some(b'b') | Some(b'B') => {
                    self.eat_byte();
                    return self.lex_non_decimal(2, str_start);
                }
                Some(b'o') | Some(b'O') => {
                    self.eat_byte();
                    return self.lex_non_decimal(8, str_start);
                }
                Some(b'x') | Some(b'X') => {
                    self.eat_byte();
                    return self.lex_non_decimal(16, str_start);
                }
                _ => {}
            }
        }
        if start == b'.' {
            return self.lex_number_mantissa(str_start);
        }
        while self
            .peek_byte()
            .as_ref()
            .map(u8::is_ascii_digit)
            .unwrap_or(false)
        {
            self.eat_byte();
        }
        let mut big = false;
        match self.peek_byte() {
            Some(b'.') => {
                self.eat_byte();
                return self.lex_number_mantissa(str_start);
            }
            Some(b'e') | Some(b'E') => {
                self.eat_byte();
                return self.lex_number_exponent(str_start);
            }
            Some(b'n') => {
                self.eat_byte();
                big = true;
            }
            _ => (),
        }
        if big {
            let s =
                str::from_utf8(&self.source.source().as_bytes()[str_start..self.offset]).unwrap();
            let s = self.interner.intern(s);
            Ok(self.token_num(Number::Big(s)))
        } else {
            self.lex_number_string(str_start)
        }
    }

    fn lex_number_string(&mut self, str_start: usize) -> LexResult<Token> {
        let num: f64 =
            lexical_core::parse(&self.source.source().as_bytes()[str_start..self.offset]).unwrap();
        if ((num as i32) as f64).to_bits() == num.to_bits() {
            Ok(self.token_num(Number::Integer(num as i32)))
        } else {
            Ok(self.token_num(Number::Float(num)))
        }
    }

    fn lex_number_mantissa(&mut self, str_start: usize) -> LexResult<Token> {
        while self
            .peek_byte()
            .as_ref()
            .map(u8::is_ascii_digit)
            .unwrap_or(false)
        {
            self.eat_byte();
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

    fn lex_number_exponent(&mut self, str_start: usize) -> LexResult<Token> {
        let start = self.peek_byte();
        if start.is_none() {
            return Err(ErrorKind::InvalidNumber);
        }
        let start = start.unwrap();
        if start == b'-' || start == b'+' || start.is_ascii_digit() {
            self.next_byte();
            while self
                .peek_byte()
                .as_ref()
                .map(u8::is_ascii_digit)
                .unwrap_or(false)
            {
                self.next_byte();
            }
            return self.lex_number_string(str_start);
        }
        Err(ErrorKind::InvalidNumber)
    }

    fn lex_non_decimal(&mut self, radix: u8, str_start: usize) -> LexResult<Token> {
        while self
            .peek_byte()
            .map(|c| is_radix(c, radix))
            .unwrap_or(false)
        {
            self.next_byte();
        }
        let mut big = false;
        if let Some(b'n') = self.peek_byte() {
            self.next_byte();
            big = true;
        }
        let s =
            str::from_utf8(&self.source.source().as_bytes()[str_start + 2..self.offset]).unwrap();
        if big {
            let s = self.interner.intern(s);
            Ok(self.token_num(Number::Big(s)))
        } else {
            //TODO dont think this is correct way of parsing.
            let num = u64::from_str_radix(s, radix as u32).map_err(|_| ErrorKind::InvalidNumber)?;
            if num as i32 as u64 == num {
                Ok(self.token_num(Number::Integer(num as i32)))
            } else {
                Ok(self.token_num(Number::Float(num as f64)))
            }
        }
    }
}
