use token::{t, Token, TokenKind};

use crate::{Lexer, Unit};

impl<'a> Lexer<'a> {
    fn lex_radix(&mut self, radix: u8) -> Token {
        //TODO This implementation is wrong for big numbers.
        let mut num = 0f64;
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
            num *= radix as f64;
            num += x as f64
        }

        let id = self.finish_number(num);
        self.finish_token(t!("num"), Some(id))
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

    fn lex_exponent(&mut self) -> bool {
        let Some(first) = self.peek_byte() else {
            // A exponent must have atleast a single digit.
            return false
        };

        if first != b'-' && first != b'+' && first.is_ascii_digit() {
            return false;
        }

        while self
            .peek_byte()
            .map(|x| x.is_ascii_digit())
            .unwrap_or(false)
        {
            self.next_unit();
        }
        true
    }

    fn lex_bigint(&mut self, start: u8, mut iter: Unit) -> Token {
        self.buffer.ascii.push(start);
        while let Some(c) = iter.next_unit() {
            let c = c as u8;
            if c == b'n' {
                break;
            }
            self.buffer.ascii.push(c);
        }
        let id = self.finish_string();
        self.finish_token(t!("big int"), Some(id))
    }

    fn parse_number(&mut self, start: u8, mut iter: Unit) -> Token {
        let len = self.end - self.start;
        self.buffer.ascii.push(start);
        for _ in 0..len - 1 {
            self.buffer.ascii.push(iter.next_unit().unwrap() as u8)
        }
        let str = unsafe { std::str::from_utf8_unchecked(&self.buffer.ascii) };
        // Should always succeed since we alread parse the number.
        let number = str.parse().unwrap();
        self.buffer.ascii.clear();
        let id = self.finish_number(number);
        self.finish_token(t!("num"), Some(id))
    }

    pub(super) fn lex_number(&mut self, start: u8) -> Token {
        // used for reparsing bigints.
        let iter = self.units.clone();

        if start == b'0' {
            match self.peek_byte() {
                None => {
                    let id = self.finish_number(0.0);
                    return self.finish_token(t!("num"), Some(id));
                }
                Some(b'n') => {
                    self.next_unit();
                    self.buffer.push(b'0' as u16);
                    let id = self.finish_string();
                    return self.finish_token(t!("big int"), Some(id));
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

        let success = if start == b'.' {
            self.lex_mantissa()
        } else {
            //TODO: figure out if parsing numbers like this can lead to precision problems
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
            return self.finish_token(TokenKind::Unknown, None);
        }

        self.parse_number(start, iter)
    }
}
