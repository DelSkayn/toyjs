#![allow(dead_code)]

use bytemuck::Pod;
use common::{
    interner::Interner,
    number::{Number, NumberId},
    span::Span,
    string::{Encoding, String, StringBuilder, StringId, Units},
    structs::Interners,
    unicode::{byte, chars, units, CharExt, Utf16Ext},
};
use token::{t, Token, TokenKind, TokenKindData};

mod ident;
mod number;
mod regex;
mod string;

/// Additional data produced during lexing which is not present in the produced token.
pub struct LexingData {
    pub strings: Interner<String, StringId>,
    pub numbers: Interner<Number, NumberId>,
}

impl LexingData {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        LexingData {
            strings: Interner::new(),
            numbers: Interner::new(),
        }
    }

    pub fn push_string(&mut self, encoding: Encoding) -> StringId {
        self.strings.intern(&encoding)
    }
}

/// The toyjs lexer, produces tokens from a source string.
pub struct Lexer<'a> {
    units: Units<'a>,
    start: usize,
    end: usize,
    peek: Option<u16>,
    overread: Option<u16>,
    builder: StringBuilder,
    pub data: &'a mut Interners,
}

impl<'a> Lexer<'a> {
    pub fn new(units: Encoding<'a>, interners: &'a mut Interners) -> Self {
        let units = units.units();

        Self {
            units,
            start: 0,
            end: 0,
            peek: None,
            overread: None,
            builder: StringBuilder::new(),
            data: interners,
        }
    }

    /// Pop the next code from the list
    #[inline]
    fn next_unit(&mut self) -> Option<u16> {
        self.end += 1;
        self.peek.take().or_else(|| self.units.next())
    }

    /// Peek the next code on the list without consuming it.
    #[inline]
    fn peek_unit(&mut self) -> Option<u16> {
        if let Some(x) = self.peek {
            Some(x)
        } else {
            self.peek = self.units.next();
            self.peek
        }
    }

    /// Peek the next code on the list without consuming it.
    #[inline]
    fn peek_byte(&mut self) -> Option<u8> {
        self.peek_unit().and_then(|x| x.try_into().ok())
    }

    /// Wrap a token kind in a span and update set the next token to start at the end of the
    /// current.
    #[inline]
    fn finish_token_inner<D: Pod>(&mut self, kind: TokenKind, id: Option<D>) -> Token {
        let span = Span::from_range(self.start..self.end);
        self.start = self.end;

        Token {
            kind_and_data: TokenKindData::new(kind, id),
            span,
        }
    }

    /// Wrap a token kind in a span and update set the next token to start at the end of the
    /// current.
    #[inline]
    fn finish_token(&mut self, kind: TokenKind) -> Token {
        self.finish_token_inner::<u32>(kind, None)
    }

    /// Wrap a token kind in a span and update set the next token to start at the end of the
    /// current.
    #[inline]
    fn finish_token_number(&mut self, kind: TokenKind, id: Option<NumberId>) -> Token {
        self.finish_token_inner(kind, id)
    }

    /// Wrap a token kind in a span and update set the next token to start at the end of the
    /// current.
    #[inline]
    fn finish_token_string(&mut self, kind: TokenKind, id: Option<StringId>) -> Token {
        self.finish_token_inner(kind, id)
    }

    /// Finish the string in the string buffer and push it into the strings data.
    /// Returns the id of the strings data.
    fn finish_string(&mut self) -> StringId {
        let result = self.builder.encoding();
        let id = self.data.strings.intern(&result);
        self.builder.clear();
        id
    }

    fn finish_number(&mut self, number: f64) -> NumberId {
        self.data.numbers.intern(&Number(number))
    }

    fn lex_slash(&mut self) -> Token {
        let byte = self.peek_byte();
        match byte {
            Some(b'/') => {
                self.next_unit();
                self.lex_comment()
            }
            Some(b'*') => {
                self.next_unit();
                self.lex_multiline_comment()
            }
            Some(b'=') => {
                self.next_unit();
                self.finish_token(t!("/="))
            }
            _ => self.finish_token(t!("/")),
        }
    }

    fn lex_closing_brace(&mut self) -> Token {
        self.finish_token(t!("}"))
    }

    fn lex_whitespace(&mut self) -> Token {
        while self
            .peek_unit()
            .map(|x| units::WHITE_SPACE_CONST.into_iter().any(|y| x == y))
            .unwrap_or(false)
        {
            self.next_unit();
        }
        self.finish_token(t!(" "))
    }

    fn lex_comment(&mut self) -> Token {
        while self
            .peek_unit()
            .map(|x| units::LINE_TERMINATOR_CONST.into_iter().all(|y| x != y))
            .unwrap_or(false)
        {
            self.next_unit();
        }
        self.finish_token(t!("//"))
    }

    fn lex_multiline_comment(&mut self) -> Token {
        const STAR: u16 = b'*' as u16;
        const SLASH: u16 = b'/' as u16;

        loop {
            match self.next_unit() {
                Some(STAR) => {
                    if let Some(SLASH) = self.next_unit() {
                        return self.finish_token(t!("//"));
                    }
                }
                Some(_) => {}
                None => return self.finish_token(TokenKind::Unknown),
            }
        }
    }

    #[inline]
    fn lex_ascii(&mut self, byte: u8) -> Token {
        let kind = match byte {
            byte::LF => t!("\n"),
            byte::CR => {
                if Some(byte::LF as u16) == self.peek_unit() {
                    self.next_unit();
                }
                t!("\n")
            }
            byte::SP | byte::TAB | byte::VT | byte::FF | byte::NBSP => {
                return self.lex_whitespace()
            }
            b'~' => t!("~"),
            b';' => t!(";"),
            b':' => t!(":"),
            b',' => t!(","),
            b'(' => t!("("),
            b')' => t!(")"),
            b'[' => t!("["),
            b']' => t!("]"),
            b'{' => t!("{"),
            b'}' => t!("}"),
            b'?' => match self.peek_byte() {
                Some(b'?') => {
                    self.next_unit();
                    t!("??")
                }
                _ => t!("?"),
            },
            b'+' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_unit();
                    t!("+=")
                }
                Some(b'+') => {
                    self.next_unit();
                    t!("++")
                }
                _ => t!("+"),
            },
            b'-' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_unit();
                    t!("-=")
                }
                Some(b'-') => {
                    self.next_unit();
                    t!("--")
                }
                _ => t!("-"),
            },
            b'*' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_unit();
                    t!("*=")
                }
                Some(b'*') => {
                    self.next_unit();
                    match self.peek_byte() {
                        Some(b'=') => {
                            self.next_unit();
                            t!("**=")
                        }
                        _ => t!("**"),
                    }
                }
                _ => t!("*"),
            },
            b'/' => match self.peek_byte() {
                Some(b'/') => {
                    self.next_unit();
                    return self.lex_comment();
                }
                Some(b'*') => {
                    self.next_unit();
                    return self.lex_multiline_comment();
                }
                Some(b'=') => {
                    self.next_unit();
                    t!("/=")
                }
                _ => t!("/"),
            },
            b'%' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_unit();
                    t!("%=")
                }
                _ => t!("%"),
            },
            b'<' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_unit();
                    t!("<=")
                }
                Some(b'<') => {
                    self.next_unit();
                    match self.peek_byte() {
                        Some(b'=') => {
                            self.next_unit();
                            t!("<<=")
                        }
                        _ => t!("<<"),
                    }
                }
                _ => t!("<"),
            },
            b'>' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_unit();
                    t!(">=")
                }
                Some(b'>') => {
                    self.next_unit();
                    match self.peek_byte() {
                        Some(b'=') => {
                            self.next_unit();
                            t!(">>=")
                        }
                        Some(b'>') => {
                            self.next_unit();
                            match self.peek_byte() {
                                Some(b'=') => {
                                    self.next_unit();
                                    t!(">>>=")
                                }
                                _ => t!(">>>"),
                            }
                        }
                        _ => t!(">>"),
                    }
                }
                _ => t!(">"),
            },
            b'=' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_unit();
                    match self.peek_byte() {
                        Some(b'=') => {
                            self.next_unit();
                            t!("===")
                        }
                        _ => t!("=="),
                    }
                }
                Some(b'>') => {
                    self.next_unit();
                    t!("=>")
                }
                _ => t!("="),
            },
            b'!' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_unit();
                    match self.peek_byte() {
                        Some(b'=') => {
                            self.next_unit();
                            t!("!==")
                        }
                        _ => t!("!="),
                    }
                }
                _ => t!("!"),
            },
            b'&' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_unit();
                    t!("&=")
                }
                Some(b'&') => {
                    self.next_unit();
                    t!("&&")
                }
                _ => t!("&"),
            },
            b'|' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_unit();
                    t!("|=")
                }
                Some(b'|') => {
                    self.next_unit();
                    t!("||")
                }
                _ => t!("|"),
            },
            b'^' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_unit();
                    t!("^=")
                }
                _ => t!("^"),
            },
            b'.' => match self.peek_byte() {
                Some(b'.') => {
                    self.next_unit();
                    match self.peek_byte() {
                        Some(b'.') => {
                            self.next_unit();
                            t!("...")
                        }
                        _ => t!(".."),
                    }
                }
                Some(x) if x.is_ascii_digit() => return self.lex_number(&[b'.', x]),
                _ => t!("."),
            },
            // These characters are not offical identifier starters according to unicode
            // but are specified by ecmascript as such.
            b'$' => return self.lex_ident('$'),
            b'_' => return self.lex_ident('_'),
            b'\\' => return self.lex_ident('\\'),
            b'\'' => return self.lex_string(b'\'' as u16),
            b'\"' => return self.lex_string(b'\"' as u16),
            b'`' => return self.lex_template(true),
            x if x.is_ascii_alphabetic() => {
                return self.lex_ident(char::from_u32(x as u32).unwrap())
            }
            x if x.is_ascii_digit() => return self.lex_number(&[x]),
            _ => TokenKind::Unknown,
        };
        self.finish_token(kind)
    }

    /// Lex a full width character, don't handle ascii characters here as they should already be
    /// handled.
    fn lex_char(&mut self, c: char) -> Token {
        debug_assert!(!c.is_ascii());

        let kind = match c {
            chars::ZWNBSP
            | '\u{1680}'
            | '\u{2000}'
            | '\u{2001}'
            | '\u{2002}'
            | '\u{2003}'
            | '\u{2004}'
            | '\u{2005}'
            | '\u{2006}'
            | '\u{2007}'
            | '\u{2008}'
            | '\u{2009}'
            | '\u{200A}'
            | '\u{202F}'
            | '\u{205F}'
            | '\u{3000}' => return self.lex_whitespace(),
            chars::LS | chars::PS => t!("\n"),
            x if x.is_xid_start() => return self.lex_ident(x),
            _ => TokenKind::Unknown,
        };
        self.finish_token(kind)
    }

    /// Redoes lexing for a `/` or `/=` token changing it to be parsed as a regex.
    pub fn relex_regex(&mut self, token: Token) -> Token {
        debug_assert!(matches!(token.kind(), t!("/") | t!("/=")));
        debug_assert_eq!(token.span.offset() + token.span.size(), self.start);
        self.start = token.span.offset();
        if let t!("/=") = token.kind() {
            self.builder.push(b'=' as u16);
            self.lex_regex(false)
        } else {
            self.lex_regex(true)
        }
    }

    /// Redoes lexing for a `}` token changing it to be parsed as a regex.
    pub fn relex_template(&mut self, token: Token) -> Token {
        debug_assert_eq!(token.kind(), t!("}"));
        debug_assert_eq!(token.span.offset() + token.span.size(), self.start);
        self.start = token.span.offset();
        self.lex_template(false)
    }

    #[inline]
    pub fn next_token(&mut self) -> Option<Token> {
        let unit = self.overread.take().or(self.next_unit())?;
        let res = if unit.is_ascii() {
            self.lex_ascii(unit as u8)
        } else {
            let char = unit.decode_utf16_with(|| {
                let Some(trailing) = self.next_unit() else {
                    // Encoding can only contain valid utf16 or ascii any other text is undefined
                    // behaviour. So this should be unreachable if safety guarentees where upheld.
                    unreachable!("lexer source data should be valid utf16.")
                };
                trailing
            });
            self.lex_char(char)
        };
        Some(res)
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod test {
    use common::{source::Source, span::Span, structs::Interners};
    use token::{t, Token, TokenKindData};

    use crate::Lexer;

    #[test]
    fn newlines() {
        let source = b"\n\n\n \n\n  tgt\n\n";
        let source = Source::new(std::str::from_utf8(source).unwrap(), Some("test"));
        let mut interners = Interners::default();
        let mut lexer = Lexer::new(source.source(), &mut interners);

        assert_eq!(
            lexer.next(),
            Some(Token {
                kind_and_data: TokenKindData::new::<u32>(t!("\n"), None),
                span: Span::new(0, 1),
            }),
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind_and_data: TokenKindData::new::<u32>(t!("\n"), None),
                span: Span::new(1, 1),
            }),
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind_and_data: TokenKindData::new::<u32>(t!("\n"), None),
                span: Span::new(2, 1),
            }),
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind_and_data: TokenKindData::new::<u32>(t!(" "), None),
                span: Span::new(3, 1),
            }),
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind_and_data: TokenKindData::new::<u32>(t!("\n"), None),
                span: Span::new(4, 1),
            }),
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind_and_data: TokenKindData::new::<u32>(t!("\n"), None),
                span: Span::new(5, 1),
            }),
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind_and_data: TokenKindData::new::<u32>(t!(" "), None),
                span: Span::new(6, 2),
            }),
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind_and_data: TokenKindData::new::<u32>(t!("ident"), Some(0)),
                span: Span::new(8, 3),
            }),
        );
    }
}
