use common::unicode::{chars, units, CharExt, Utf16Ext};
use token::{Token, TokenKind};

use crate::Lexer;

const SLASH: u16 = b'\\' as u16;
const DASH: u16 = b'/' as u16;
const STAR: u16 = b'*' as u16;
const BRACKET_OPEN: u16 = b'[' as u16;
const BRACKET_CLOSE: u16 = b']' as u16;

impl<'a> Lexer<'a> {
    pub(crate) fn lex_regex(&mut self, mut first: bool) -> Token {
        loop {
            let Some(unit) = self.next_unit() else {
                return self.finish_token(TokenKind::Unknown);
            };

            // First character can't be / or *
            // Line terminator not allowed.
            match unit {
                DASH | STAR if first => {
                    return self.finish_token(TokenKind::Unknown);
                }
                DASH => {
                    self.builder.push(DASH);
                    break;
                }
                units::LF | units::CR | units::LS | units::PS => {
                    return self.finish_token(TokenKind::Unknown)
                }
                BRACKET_OPEN => {
                    if !self.lex_regex_class() {
                        return self.finish_token(TokenKind::Unknown);
                    }
                }
                SLASH => {
                    if !self.lex_regex_backslash() {
                        return self.finish_token(TokenKind::Unknown);
                    }
                }
                _ => {
                    self.builder.push(unit);
                    if unit.is_utf16_leading_surrogate() {
                        let Some(unit) = self.next_unit() else {
                            return self.finish_token(TokenKind::Unknown);
                        };
                        self.builder.push(unit);
                    }
                }
            }
            first = false;
        }

        loop {
            let Some(unit) = self.next_unit() else {
                break;
            };
            let char = unit.decode_utf16_with(|| self.next_unit().expect("invalid utf16"));
            if !char.is_xid_continue() && char != '$' && char != chars::ZWNJ && char != chars::ZWJ {
                // We read a unit to much so put it back for the next token.
                if let (lead, Some(trail)) = char.encode_utf16_code_point() {
                    self.overread = Some(lead);
                    self.end -= 2;
                    self.peek = Some(trail);
                } else {
                    self.end -= 1;
                    self.peek = Some(unit);
                }
                break;
            }
            let (lead, trail) = char.encode_utf16_code_point();
            self.builder.push(lead);
            if let Some(trail) = trail {
                self.builder.push(trail);
            }
        }

        self.finish_token_string(TokenKind::Regex)
    }

    fn lex_regex_class(&mut self) -> bool {
        self.builder.push(BRACKET_OPEN);
        loop {
            let Some(unit) = self.next_unit() else {
                return false;
            };
            match unit {
                BRACKET_CLOSE => return true,
                units::LF | units::CR | units::LS | units::PS => return false,
                SLASH => {
                    if !self.lex_regex_backslash() {
                        return false;
                    }
                }
                _ => {
                    self.builder.push(unit);
                    if unit.is_utf16_leading_surrogate() {
                        let Some(unit) = self.next_unit() else {
                            return false;
                        };
                        self.builder.push(unit);
                    }
                }
            }
        }
    }

    fn lex_regex_backslash(&mut self) -> bool {
        self.builder.push(SLASH);
        let Some(unit) = self.next_unit() else {
            return false;
        };
        match unit {
            units::LF | units::CR | units::LS | units::PS => return false,
            _ => {
                self.builder.push(unit);
                if unit.is_utf16_leading_surrogate() {
                    let Some(unit) = self.next_unit() else {
                        return false;
                    };
                    self.builder.push(unit);
                }
            }
        }
        true
    }
}
