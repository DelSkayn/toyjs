use common::string::Units;
use token::{t, Token, TokenKind};

use crate::Lexer;

impl<'a> Lexer<'a> {
    fn lex_radix(&mut self, radix: u8) -> Token {
        // Implementation derived from node.
        while let Some(x) = self.peek_byte() {
            if x != b'0' {
                break;
            }
            self.next_unit();
        }

        let mut number: u64 = 0;
        let mut exponent: u32 = 0;

        while let Some(x) = self
            .peek_byte()
            .and_then(|x| {
                if x.is_ascii() {
                    char::from_u32(x as u32)
                } else {
                    None
                }
            })
            .and_then(|x| x.to_digit(radix as u32))
        {
            self.next_unit();
            number = number * (radix as u64) + (x as u64);
            let mut overflow = number >> 53;
            if overflow > 0 {
                let mut overflow_bits = 1i32;
                while overflow > 1 {
                    overflow_bits += 1;
                    overflow >>= 1;
                }

                let dropped_bits_mask = (1 << overflow_bits) - 1;
                let dropped_bits = (number as i32) & dropped_bits_mask;
                number >>= overflow_bits;
                exponent = overflow_bits as u32;

                let mut zero_tail = true;
                while let Some(x) = self
                    .peek_byte()
                    .and_then(|x| {
                        if x.is_ascii() {
                            char::from_u32(x as u32)
                        } else {
                            None
                        }
                    })
                    .and_then(|x| x.to_digit(radix as u32))
                {
                    self.next_unit();
                    zero_tail = zero_tail && x == 0;
                    exponent += radix.ilog2();
                }

                let middle_value = 1 << (overflow_bits - 1);
                if dropped_bits > middle_value
                    || dropped_bits == middle_value && ((number & 1) != 0 || !zero_tail)
                {
                    number += 1;
                }

                if number & (1u64 << 53) != 0 {
                    exponent += 1;
                    number >>= 1;
                }
                break;
            }
        }

        debug_assert!(number < 1u64 << 53);
        debug_assert!(number as f64 as u64 == number);

        if exponent == 0 {
            let id = self.finish_number(number as f64);
            self.finish_token_number(t!("num"), Some(id))
        } else {
            let number = (number as f64) * 2f64.powi(exponent as i32);
            let id = self.finish_number(number);
            self.finish_token_number(t!("num"), Some(id))
        }
    }

    /// Lex the mantissa or the '.345' part of number '12.345'
    fn lex_mantissa(&mut self) -> bool {
        let mut once = false;
        while self
            .peek_byte()
            .map(|x| x.is_ascii_digit())
            .unwrap_or(false)
        {
            once = true;
            self.next_unit();
        }
        // A mantissa must have atleast a single digit.
        if !once {
            return false;
        }
        if let Some(b'e' | b'E') = self.peek_byte() {
            self.next_unit();
            return self.lex_exponent();
        }
        true
    }

    /// Lex the exponent or `e-34` parr for number `12e-34`
    fn lex_exponent(&mut self) -> bool {
        let Some(first) = self.peek_byte() else {
            // A exponent must have atleast a single digit.
            return false
        };

        if first != b'-' && first != b'+' && !first.is_ascii_digit() {
            return false;
        }

        self.next_unit();

        while self
            .peek_byte()
            .map(|x| x.is_ascii_digit())
            .unwrap_or(false)
        {
            self.next_unit();
        }
        true
    }

    fn lex_bigint(&mut self, start: &[u8], iter: Units) -> Token {
        for s in start {
            self.builder.ascii.push(*s);
        }
        for c in iter {
            let c = c as u8;
            if c == b'n' {
                break;
            }
            self.builder.ascii.push(c);
        }
        let id = self.finish_string();
        self.finish_token_string(t!("big int"), Some(id))
    }

    fn parse_number(&mut self, start: &[u8], mut iter: Units) -> Token {
        let len = self.end - self.start;
        debug_assert!(self.builder.ascii.is_empty());
        for s in start {
            self.builder.ascii.push(*s);
        }
        let buf_len = self.builder.ascii.len();
        for _ in buf_len..len {
            // Unwrap because we should have already parsed the bytes.
            let char = iter.next().unwrap() as u8;
            // Should be valid as the we already lexed the number.
            debug_assert!(char.is_ascii());
            self.builder.ascii.push(char)
        }
        // SAFETY: We pushed only ascii characters so the buffer contains a valid utf-8 string.
        let str = unsafe { std::str::from_utf8_unchecked(&self.builder.ascii) };
        // Should always succeed since we alread parse the number.
        let Ok(number) = str.parse() else {
            panic!("invalid number: {} at {}",str,self.end);
        };
        self.builder.ascii.clear();
        let id = self.finish_number(number);
        self.finish_token_number(t!("num"), Some(id))
    }

    pub(super) fn lex_number(&mut self, start: &[u8]) -> Token {
        // used for reparsing bigints.
        let iter = self.units.clone();

        if start[0] == b'0' {
            match self.peek_byte() {
                None => {
                    let id = self.finish_number(0.0);
                    return self.finish_token_number(t!("num"), Some(id));
                }
                Some(b'n') => {
                    self.next_unit();
                    self.builder.push(b'0' as u16);
                    let id = self.finish_string();
                    return self.finish_token_string(t!("big int"), Some(id));
                }
                Some(b'b' | b'B') => {
                    self.next_unit();
                    return self.lex_radix(2);
                }
                Some(b'o' | b'O') => {
                    self.next_unit();
                    return self.lex_radix(8);
                }
                Some(b'x' | b'X') => {
                    self.next_unit();
                    return self.lex_radix(16);
                }
                _ => {}
            }
        }

        let success = if start[0] == b'.' {
            self.lex_mantissa()
        } else {
            while self
                .peek_byte()
                .map(|x| x.is_ascii_digit())
                .unwrap_or(false)
            {
                self.next_unit();
            }

            match self.peek_byte() {
                Some(b'.') => {
                    self.next_unit();
                    self.lex_mantissa()
                }
                Some(b'e' | b'E') => {
                    self.next_unit();
                    self.lex_exponent()
                }
                Some(b'n') => {
                    self.next_unit();
                    return self.lex_bigint(start, iter);
                }
                _ => true,
            }
        };

        if !success {
            return self.finish_token(TokenKind::Unknown);
        }

        self.parse_number(start, iter)
    }
}
