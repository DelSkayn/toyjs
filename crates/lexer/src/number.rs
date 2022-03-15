use super::*;
use token::{Literal, Number, TokenKind};

impl<'a> Lexer<'a> {
    fn token_num(&mut self, kind: Number) -> Token {
        self.token(TokenKind::Literal(Literal::Number(kind)))
    }

    /// Lex a number
    ///
    /// Start should be the first byte of the number.
    pub(super) fn lex_number(&mut self, start: u8) -> LexResult<Token> {
        let str_start = self.offset - 1;

        if start == b'0' {
            // Number might be a octal, binary, hex, or big number
            match self.peek_byte() {
                None => return Ok(self.token_num(Number::Integer(0))),
                // Number is a big int, Very much TODO.
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
            // Number has a fractional part.
            return self.lex_number_mantissa(str_start);
        }

        // Lex the integer part.
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
                // Number has a fractional part.
                self.eat_byte();
                return self.lex_number_mantissa(str_start);
            }
            Some(b'e') | Some(b'E') => {
                // Number has an exponent.
                self.eat_byte();
                return self.lex_number_exponent(str_start);
            }
            Some(b'n') => {
                // Number is a bigint.
                self.eat_byte();
                big = true;
            }
            _ => (),
        }

        if big {
            // We have no good way to lex big numbers yet so for now we just handle them with
            // strings.
            let s = &self.source.source()[str_start..self.offset];
            let s = self.interner.intern(s);
            Ok(self.token_num(Number::Big(s)))
        } else {
            self.lex_number_string(str_start)
        }
    }

    /// Parses the portion of the source string that number contains as a integer.
    ///
    /// For now we use a external crate to parse the number. This libraries might parse
    /// numbers different then that spec requires but for now it is good enough.
    ///
    /// `str_start` should be the byte offset of the start of the number token.
    fn lex_number_string(&mut self, str_start: usize) -> LexResult<Token> {
        //TODO Check lexical_core's compatiblity with ECMA spec.
        let num: f64 =
            lexical_core::parse(&self.source.source().as_bytes()[str_start..self.offset]).unwrap();
        // Check if the number fits in a i32 the javascript integer type.
        if ((num as i32) as f64).to_bits() == num.to_bits() {
            Ok(self.token_num(Number::Integer(num as i32)))
        } else {
            Ok(self.token_num(Number::Float(num)))
        }
    }

    /// Lex a numbers mantissa ie the `.345` part of a number like `12.345`
    /// The `.` should already be eaten when this is called.
    ///
    /// `str_start` should be the byte offset of the start of the number token.
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
                // Number has a exponent.
                self.next_byte();
                return self.lex_number_exponent(str_start);
            }
            _ => {}
        }
        self.lex_number_string(str_start)
    }

    /// Lex a numbers exponent ie the `E23` part of a number like `1E23`
    /// The `E` or `e` should already be eaten when this is called.
    ///
    /// `str_start` should be the byte offset of the start of the number token.
    fn lex_number_exponent(&mut self, str_start: usize) -> LexResult<Token> {
        let start = self.peek_byte();
        if start.is_none() {
            return Err(ErrorKind::InvalidNumber);
        }
        let start = start.unwrap();
        // An exponent number can start with `-`, `+` or a digit.
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

    /// Lexes a hex, octal, or binary number.
    /// Radix should be the radix of the number to be parsed ie 16,8, or 2 respectively.
    ///
    /// `str_start` should be the byte offset of the start of the number token.
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
        let s = &self.source.source()[str_start + 2..self.offset];
        if big {
            let s = self.interner.intern(s);
            Ok(self.token_num(Number::Big(s)))
        } else {
            //TODO dont think this is correct way of parsing.
            //TODO Not sure what past me thought was incorrect here, figure that out.
            let num = u64::from_str_radix(s, radix as u32).map_err(|_| ErrorKind::InvalidNumber)?;
            if num as i32 as u64 == num {
                Ok(self.token_num(Number::Integer(num as i32)))
            } else {
                Ok(self.token_num(Number::Float(num as f64)))
            }
        }
    }
}
