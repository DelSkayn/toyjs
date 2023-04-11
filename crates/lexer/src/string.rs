use common::unicode::{units, CharExt, Utf16Ext};
use token::{t, Template, Token, TokenKind};

use crate::Lexer;

impl<'a> Lexer<'a> {
    fn unit_to_hex_digit(unit: u16) -> Option<u8> {
        if unit.is_ascii() {
            let unit = unsafe { char::from_u32_unchecked(unit as u32) };
            return unit.to_digit(16).map(|x| x as u8);
        }
        None
    }

    pub(super) fn lex_escape_unicode(&mut self) -> bool {
        let Some(first) = self.peek_unit() else {
            return false
        };
        if first == b'{' as u16 {
            self.next_unit();
            let mut mv = 0u32;
            while let Some(x) = self.next_unit() {
                let v = if let Some(v) = Self::unit_to_hex_digit(x) {
                    v
                } else if x == b'}' as u16 {
                    break;
                } else {
                    return false;
                };
                mv <<= 4;
                mv += v as u32;
                if mv > 0x10FFFF {
                    return false;
                }
            }
            let Some(char) = char::from_u32(mv) else {
                return false
            };
            let (lead, trail) = char.encode_utf16_code_point();
            self.buffer.push(lead);
            if let Some(trail) = trail {
                self.buffer.push(trail);
            }
            true
        } else {
            let Some(first) = self.peek_unit().and_then(Self::unit_to_hex_digit) else {
                return false
            };
            self.next_unit();
            let Some(second) = self.peek_unit().and_then(Self::unit_to_hex_digit) else {
                return false
            };
            self.next_unit();
            let Some(third) = self.peek_unit().and_then(Self::unit_to_hex_digit) else {
                return false
            };
            self.next_unit();
            let Some(fourth) = self.peek_unit().and_then(Self::unit_to_hex_digit) else {
                return false
            };
            self.next_unit();
            let first = first as u32;
            let second = second as u32;
            let third = third as u32;
            let fourth = fourth as u32;
            if let Some(x) = char::from_u32(first << 12 | second << 8 | third << 4 | fourth) {
                let (lead, trail) = x.encode_utf16_code_point();
                self.buffer.push(lead);
                if let Some(trail) = trail {
                    self.buffer.push(trail);
                }
                true
            } else {
                false
            }
        }
    }

    fn lex_escape_hex(&mut self) -> bool {
        let Some(first) = self.peek_unit().and_then(Self::unit_to_hex_digit) else {
            return false
        };
        self.next_unit();
        let Some(last) = self.peek_unit().and_then(Self::unit_to_hex_digit) else {
            return false
        };
        self.next_unit();
        let first = first as u16;
        let last = last as u16;
        self.buffer.push(first << 4 | last);
        true
    }

    pub fn lex_escape(&mut self) -> bool {
        const SINGLE: u16 = b'\'' as u16;
        const DOUBLE: u16 = b'\"' as u16;
        const SLASH: u16 = b'\\' as u16;
        const B: u16 = b'b' as u16;
        const T: u16 = b't' as u16;
        const N: u16 = b'n' as u16;
        const V: u16 = b'v' as u16;
        const F: u16 = b'f' as u16;
        const R: u16 = b'r' as u16;
        const X: u16 = b'x' as u16;
        const U: u16 = b'u' as u16;

        let Some(unit) = self.next_unit() else {
            return false
        };
        match unit {
            units::LF | units::CR | units::LS | units::PS => return true,
            SINGLE => self.buffer.push(SINGLE),
            DOUBLE => self.buffer.push(DOUBLE),
            SLASH => self.buffer.push(SLASH),
            B => self.buffer.push(0x8),
            T => self.buffer.push(units::TAB),
            N => self.buffer.push(units::LF),
            V => self.buffer.push(units::VT),
            F => self.buffer.push(units::FF),
            R => self.buffer.push(units::CR),
            X => return self.lex_escape_hex(),
            U => return self.lex_escape_hex(),
            x => {
                self.buffer.push(x);
                return true;
            }
        }
        true
    }

    pub(super) fn lex_string(&mut self, start: u16) -> Token {
        const ESCAPE: u16 = b'\\' as u16;

        loop {
            let Some(unit) = self.next_unit() else {
                self.buffer.clear();
                return self.finish_token(TokenKind::Unknown)
            };

            if unit == start {
                let id = self.finish_string();
                return self.finish_token_string(t!("string"), Some(id));
            }

            match unit {
                units::CR => {
                    self.buffer.clear();
                    return self.finish_token(TokenKind::Unknown);
                }
                units::LF => {
                    self.buffer.clear();
                    return self.finish_token(TokenKind::Unknown);
                }
                ESCAPE => {
                    if !self.lex_escape() {
                        self.buffer.clear();
                        return self.finish_token(TokenKind::Unknown);
                    }
                }
                x => self.buffer.push(x),
            }
        }
    }

    pub(super) fn lex_template(&mut self, is_start: bool) -> Token {
        const DOLLAR: u16 = b'$' as u16;
        const CLOSE: u16 = b'`' as u16;
        const ESCAPE: u16 = b'\\' as u16;
        const OPEN_BRACKET: u16 = b'{' as u16;

        while let Some(x) = self.next_unit() {
            match x {
                DOLLAR => {
                    if let Some(OPEN_BRACKET) = self.peek_unit() {
                        self.next_unit();
                        let template = if is_start {
                            Template::Start
                        } else {
                            Template::Middle
                        };
                        let id = self.finish_string();
                        return self.finish_token_string(TokenKind::Template(template), Some(id));
                    } else {
                        self.buffer.push(x);
                    }
                }
                CLOSE => {
                    let template = if is_start {
                        Template::NoSubstitute
                    } else {
                        Template::End
                    };
                    let id = self.finish_string();
                    return self.finish_token_string(TokenKind::Template(template), Some(id));
                }
                ESCAPE => {
                    if !self.lex_escape() {
                        return self.finish_token(TokenKind::Unknown);
                    }
                }
                x => self.buffer.push(x),
            }
        }
        self.finish_token(TokenKind::Unknown)
    }
}
