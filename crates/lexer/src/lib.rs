#![allow(dead_code)]

use std::hash::{Hash, Hasher};

use bytemuck::Pod;
use common::{
    interner::Interner,
    span::Span,
    string::{Ascii, Encoding, String, Units, Utf16},
    unicode::{byte, chars, units, CharExt, Utf16Ext},
};
use token::{t, NumberId, StringId, Token, TokenKind, TokenKindData};

mod ident;
mod number;
mod string;

/// Lexer state.
///
/// Javascript syntax can't be parsed by a stateless lexer as there are a few tokens who's prefix
/// are the same and are only parsed in certain context. The first is the character `/` as this is
/// the division operator, start of a line comment, as well as start of a regex definition. The
/// second is `}` as this can be a delimitator as well as the end of a expression in a template.
/// This enum defines how the lexer will lex tokens.
#[derive(Clone, Copy, Eq, PartialEq)]
pub enum State {
    /// Normal parsing state.
    Base,
    /// Parse the next `/` as a regex.
    Regex,
    /// Parse the next `}` as the end of a template substitute
    Template,
}

/// A wrapper around f64 which implements bitwise equility and hashing.
pub struct Number(pub f64);
impl From<&Number> for Number {
    fn from(value: &Number) -> Self {
        Number(value.0)
    }
}

impl Hash for Number {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state)
    }
}
impl Eq for Number {}
impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_bits() == other.0.to_bits()
    }
}

/// Additional data produced during lexing which is not present in the produced token.
pub struct LexingData {
    pub strings: Interner<String, StringId>,
    pub numbers: Interner<Number, NumberId>,
}

impl LexingData {
    pub fn new() -> Self {
        LexingData {
            strings: Interner::new(),
            numbers: Interner::new(),
        }
    }

    pub fn push_string(&mut self, encoding: Encoding) -> StringId {
        // Don't bother for large strings, they are unlikely to be repeated in source code.
        if encoding.len() > 32 {
            self.strings.skip_push(encoding)
        } else {
            self.strings.intern(&encoding)
        }
    }
}

struct LexBuffer {
    ascii: Vec<u8>,
    utf16: Vec<u16>,
    is_ascii: bool,
}

impl LexBuffer {
    pub fn new() -> Self {
        Self {
            ascii: Vec::new(),
            utf16: Vec::new(),
            is_ascii: true,
        }
    }

    pub fn push(&mut self, unit: u16) {
        if self.is_ascii {
            if unit.is_ascii() {
                self.ascii.push(unit as u8);
            } else {
                self.utf16.reserve(self.ascii.len());
                self.ascii.drain(..).for_each(|x| self.utf16.push(x as u16));
                self.is_ascii = false;
                self.utf16.push(unit);
            }
        } else {
            self.utf16.push(unit);
        }
    }

    pub fn take(&mut self) -> String {
        if self.is_ascii {
            let ascii = unsafe { Ascii::from_slice_unchecked(&self.ascii) };
            let res = String::from(ascii);
            self.ascii.clear();
            res
        } else {
            let ascii = unsafe { Utf16::from_slice_unchecked(&self.utf16) };
            let res = String::from(ascii);
            self.utf16.clear();
            self.is_ascii = true;
            res
        }
    }

    pub fn encoding(&self) -> Encoding {
        unsafe {
            if self.is_ascii {
                Encoding::Ascii(Ascii::from_slice_unchecked(&self.ascii))
            } else {
                Encoding::Utf16(Utf16::from_slice_unchecked(&self.utf16))
            }
        }
    }

    pub fn clear(&mut self) {
        if self.is_ascii {
            self.ascii.clear()
        } else {
            self.is_ascii = true;
            self.utf16.clear()
        }
    }
}

impl Default for LexBuffer {
    fn default() -> Self {
        Self::new()
    }
}

/// The toyjs lexer, produces tokens from a source string.
pub struct Lexer<'a> {
    units: Units<'a>,
    start: usize,
    end: usize,
    states: Vec<State>,
    peek: Option<u16>,
    overread: Option<u16>,
    buffer: LexBuffer,
    pub data: LexingData,
}

impl<'a> Lexer<'a> {
    pub fn new(units: Encoding<'a>) -> Self {
        let units = units.units();

        Self {
            units,
            start: 0,
            end: 0,
            states: vec![State::Base],
            peek: None,
            overread: None,
            buffer: LexBuffer::new(),
            data: LexingData::new(),
        }
    }

    /// Push a new state into the lexer.
    #[inline]
    pub fn push_state(&mut self, state: State) {
        self.states.push(state)
    }

    /// Pop the current state from the lexer.
    /// You can't pop the last state, trying to do so will result in a panic.
    #[inline]
    pub fn pop_state(&mut self) -> State {
        assert!(self.states.len() > 1, "tried to pop last state in lexer");
        self.states.pop().unwrap()
    }

    /// The current state of the lexer
    #[inline]
    pub fn state(&self) -> State {
        *self.states.last().unwrap()
    }

    /// Pop the next code from the list
    fn next_unit(&mut self) -> Option<u16> {
        self.end += 1;
        self.peek.take().or_else(|| self.units.next())
    }

    /// Peek the next code on the list without consuming it.
    fn peek_unit(&mut self) -> Option<u16> {
        if let Some(x) = self.peek {
            Some(x)
        } else {
            self.peek = self.units.next();
            self.peek
        }
    }

    /// Peek the next code on the list without consuming it.
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
        let result = self.buffer.encoding();
        let id = self.data.push_string(result);
        self.buffer.clear();
        id
    }

    fn finish_number(&mut self, number: f64) -> NumberId {
        self.data.numbers.intern(&Number(number))
    }

    fn lex_regex(&mut self) -> Token {
        todo!()
    }

    fn lex_slash(&mut self) -> Token {
        let byte = self.peek_byte();
        match byte {
            Some(b'/') => {
                self.next_unit();
                return self.lex_comment();
            }
            Some(b'*') => {
                self.next_unit();
                return self.lex_multiline_comment();
            }
            _ => {}
        };
        match self.state() {
            State::Regex => self.lex_regex(),
            _ => match byte {
                Some(b'=') => {
                    self.next_unit();
                    self.finish_token(t!("/="))
                }
                _ => self.finish_token(t!("/")),
            },
        }
    }

    fn lex_closing_brace(&mut self) -> Token {
        if let State::Template = self.state() {
            self.lex_template(false)
        } else {
            self.finish_token(t!("}"))
        }
    }

    fn lex_whitespace(&mut self) -> Token {
        while self
            .peek_unit()
            .map(|x| units::WHITE_SPACE_CONST.into_iter().any(|y| x == y))
            .unwrap_or(false)
        {
            self.next_token();
        }
        self.finish_token(t!(" "))
    }

    fn lex_comment(&mut self) -> Token {
        self.next_token();
        while self
            .peek_unit()
            .map(|x| units::LINE_TERMINATOR_CONST.into_iter().all(|y| x != y))
            .unwrap_or(false)
        {
            self.next_token();
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
            b'}' => return self.lex_closing_brace(),
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
            b'/' => return self.lex_slash(),
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

            x if x.is_ascii_digit() => return self.lex_number(&[x]),
            x if x.is_ascii_alphabetic() => {
                return self.lex_ident(char::from_u32(x as u32).unwrap())
            }
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

    #[inline]
    pub fn next_token(&mut self) -> Option<Token> {
        let unit = self.overread.take().or(self.next_unit())?;
        let res = if unit.is_ascii() {
            self.lex_ascii(unit as u8)
        } else {
            let char = unit.decode_utf16_with(|| {
                let Some(trailing) = self.next_unit() else{
                    // Encoding can only contain valid utf16 or ascii any other text is undefined
                    // behaviour. So this should be unreachable if safety guarentees where upheld.
                    unreachable!("lexer source data shoulb be valid utf16.")
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
