#![allow(dead_code)]

use common::{
    interner::Interner,
    source::{Source, Span},
};
use std::{convert::TryFrom, result::Result as StdResult};
use token::{t, Keyword, Token, TokenKind};
use unicode_xid::UnicodeXID;

mod chars;
mod number;
mod string;
mod utf;

#[derive(Clone, Copy, Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub origin: Span,
}

#[derive(Clone, Copy, Debug)]
pub enum ErrorKind {
    InvalidToken,
    InvalidNumber,
    UnexpectedEnd,
    UnClosedString,
    InvalidUnicodeSequence,
    InvalidEscapeCode,
}

type LexResult<T> = StdResult<T, ErrorKind>;

pub type Result<T> = StdResult<T, Error>;

pub struct Lexer<'a> {
    source: &'a Source,
    interner: &'a mut Interner,
    offset: usize,
    span_start: usize,
    // A buffer for rewriting escaped utf-8 secuences as the right bytes
    buffer: String,
}

/// Util function to check if a byte contains a digit of a certain radix.
/// Only suports radix 2, 8 and 16 i.e. binary, octal, and hexidecimal
fn is_radix(byte: u8, radix: u8) -> bool {
    match radix {
        2 => byte == b'0' || byte == b'1',
        8 => byte >= b'0' && byte <= b'7',
        16 => byte.is_ascii_hexdigit(),
        _ => panic!("invalid radix"),
    }
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a Source, interner: &'a mut Interner) -> Self {
        Lexer {
            source,
            interner,
            offset: 0,
            span_start: 0,
            buffer: String::new(),
        }
    }

    fn token(&mut self, kind: TokenKind) -> Token {
        let hi = u32::try_from(self.offset).expect("source file to big for span");
        let low = u32::try_from(self.span_start).unwrap();
        self.span_start = self.offset;
        Token {
            kind,
            span: Span { hi, low },
        }
    }

    fn lex_line_comment(&mut self) -> LexResult<()> {
        loop {
            match self.next_byte() {
                Some(chars::LF) => return Ok(()),
                Some(chars::CR) => {
                    match self.peek_byte() {
                        Some(chars::LF) => self.eat_byte(),
                        _ => {}
                    }
                    return Ok(());
                }
                Some(x) if !x.is_ascii() => match self.next_char(x)? {
                    chars::LS | chars::PS | chars::BS => return Ok(()),
                    _ => {}
                },
                None => return Ok(()),
                _ => {}
            }
        }
    }

    fn lex_multi_line_comment(&mut self) -> LexResult<()> {
        loop {
            match self.next_byte() {
                Some(b'*') => match self.next_byte() {
                    Some(b'/') => {
                        self.span_start = self.offset;
                        return Ok(());
                    }
                    None => return Err(ErrorKind::UnexpectedEnd),
                    _ => {}
                },
                None => return Err(ErrorKind::UnexpectedEnd),
                _ => {}
            }
        }
    }

    fn lex_whitespace(&mut self) -> LexResult<()> {
        loop {
            let next = self.peek_byte();
            if next.is_none() {
                break;
            }
            match next.unwrap() {
                chars::TAB | chars::VT | chars::FF | chars::SP | chars::NBSP => self.eat_byte(),
                x if !x.is_ascii() => match self.peek_char(x)? {
                    chars::ZWNBSP | chars::ZWNJ | chars::ZWJ => {
                        self.next_char(x).unwrap();
                    }
                    _ => break,
                },
                _ => break,
            }
        }

        self.span_start = self.offset;
        Ok(())
    }

    fn match_keyword(ident: &str) -> Option<Keyword> {
        let res = match ident {
            "await" => Keyword::Await,
            "break" => Keyword::Break,
            "case" => Keyword::Case,
            "catch" => Keyword::Catch,
            "class" => Keyword::Class,
            "const" => Keyword::Const,
            "continue" => Keyword::Continue,
            "debugger" => Keyword::Debugger,
            "default" => Keyword::Default,
            "delete" => Keyword::Delete,
            "do" => Keyword::Do,
            "else" => Keyword::Else,
            "enum" => Keyword::Enum,
            "export" => Keyword::Export,
            "extends" => Keyword::Extends,
            "false" => Keyword::False,
            "finally" => Keyword::Finally,
            "for" => Keyword::For,
            "function" => Keyword::Function,
            "if" => Keyword::If,
            "import" => Keyword::Import,
            "in" => Keyword::In,
            "instanceof" => Keyword::Instanceof,
            "let" => Keyword::Let,
            "new" => Keyword::New,
            "null" => Keyword::Null,
            "return" => Keyword::Return,
            "super" => Keyword::Super,
            "switch" => Keyword::Switch,
            "this" => Keyword::This,
            "throw" => Keyword::Throw,
            "true" => Keyword::True,
            "try" => Keyword::Try,
            "typeof" => Keyword::Typeof,
            "var" => Keyword::Var,
            "void" => Keyword::Void,
            "while" => Keyword::While,
            "with" => Keyword::With,
            _ => return None,
        };
        Some(res)
    }

    fn lex_ident(&mut self, start: usize) -> LexResult<Token> {
        loop {
            match self.peek_byte() {
                Some(b'_') => self.eat_byte(),
                Some(x) if x.is_ascii_alphabetic() => self.eat_byte(),
                Some(x) if !x.is_ascii() => {
                    if self.peek_char(x)?.is_xid_continue() {
                        self.next_char(x).unwrap();
                    } else {
                        break;
                    }
                }
                Some(_) => break,
                None => break,
            }
        }
        if let Some(x) = Self::match_keyword(&self.source.source()[start..self.offset]) {
            return Ok(self.token(TokenKind::Keyword(x)));
        } else {
            let string_id = self
                .interner
                .intern(&self.source.source()[start..self.offset]);
            Ok(self.token(TokenKind::Ident(string_id)))
        }
    }

    pub fn next(&mut self) -> Result<Option<Token>> {
        self.next_inner().map_err(|e| Error {
            kind: e,
            origin: Span {
                hi: u32::try_from(self.offset).expect("source file to big for span"),
                low: u32::try_from(self.span_start).unwrap(),
            },
        })
    }

    pub fn next_inner(&mut self) -> LexResult<Option<Token>> {
        let byte = if let Some(x) = self.next_byte() {
            x
        } else {
            return Ok(None);
        };
        let token = match byte {
            chars::LF => self.token(t!("\n")),
            chars::CR => {
                match self.peek_byte() {
                    Some(chars::LF) => self.eat_byte(),
                    _ => {}
                }
                self.token(t!("\n"))
            }
            chars::TAB | chars::VT | chars::FF | chars::SP | chars::NBSP => {
                self.lex_whitespace()?;
                return self.next_inner();
            }
            b';' => self.token(t!(";")),
            b':' => self.token(t!(":")),
            b',' => self.token(t!(",")),
            b'(' => self.token(t!("(")),
            b')' => self.token(t!(")")),
            b'{' => self.token(t!("{")),
            b'}' => self.token(t!("}")),
            b'[' => self.token(t!("[")),
            b']' => self.token(t!("]")),
            b'?' => match self.peek_byte() {
                Some(b'?') => {
                    self.eat_byte();
                    self.token(t!("??"))
                }
                Some(b'.') => {
                    self.eat_byte();
                    todo!()
                    /*
                    if self.peek_byte().map(Self::is_digit).unwrap_or(false) {
                        self.cur -= 1;
                        self.token(t!("?"))
                    } else {
                        self.token(t!("?."))
                    }
                    */
                }
                _ => self.token(t!("?")),
            },
            b'+' => match self.peek_byte() {
                Some(b'=') => {
                    self.eat_byte();
                    self.token(t!("+="))
                }
                Some(b'+') => {
                    self.eat_byte();
                    self.token(t!("++"))
                }
                _ => self.token(t!("+")),
            },
            b'-' => match self.peek_byte() {
                Some(b'=') => {
                    self.eat_byte();
                    self.token(t!("-="))
                }
                Some(b'-') => {
                    self.eat_byte();
                    self.token(t!("--"))
                }
                _ => self.token(t!("-")),
            },

            b'*' => match self.peek_byte() {
                Some(b'=') => {
                    self.eat_byte();
                    self.token(t!("*"))
                }
                Some(b'*') => {
                    self.eat_byte();
                    match self.peek_byte() {
                        Some(b'=') => {
                            self.eat_byte();
                            self.token(t!("**="))
                        }
                        _ => self.token(t!("**")),
                    }
                }
                _ => self.token(t!("*")),
            },

            b'/' => match self.peek_byte() {
                Some(b'=') => {
                    self.eat_byte();
                    self.token(t!("/="))
                }
                Some(b'/') => {
                    self.lex_line_comment()?;
                    return self.next_inner();
                }
                Some(b'*') => {
                    self.lex_multi_line_comment()?;
                    return self.next_inner();
                }
                _ => self.token(t!("/")),
            },
            b'~' => self.token(t!("~")),
            b'%' => match self.peek_byte() {
                Some(b'=') => {
                    self.eat_byte();
                    self.token(t!("%="))
                }
                _ => self.token(t!("%")),
            },
            b'<' => match self.peek_byte() {
                Some(b'=') => {
                    self.eat_byte();
                    self.token(t!("<="))
                }
                Some(b'<') => {
                    self.eat_byte();
                    match self.peek_byte() {
                        Some(b'=') => {
                            self.eat_byte();
                            self.token(t!("<<="))
                        }
                        _ => self.token(t!("<<")),
                    }
                }
                _ => self.token(t!("<")),
            },
            b'>' => match self.peek_byte() {
                Some(b'=') => {
                    self.eat_byte();
                    self.token(t!(">="))
                }
                Some(b'>') => {
                    self.eat_byte();
                    match self.peek_byte() {
                        Some(b'=') => {
                            self.eat_byte();
                            self.token(t!(">>="))
                        }
                        Some(b'>') => {
                            self.eat_byte();
                            match self.peek_byte() {
                                Some(b'=') => {
                                    self.eat_byte();
                                    self.token(t!(">>>="))
                                }
                                _ => self.token(t!(">>>")),
                            }
                        }
                        _ => self.token(t!(">>")),
                    }
                }
                _ => self.token(t!(">")),
            },
            b'=' => match self.peek_byte() {
                Some(b'=') => {
                    self.eat_byte();
                    match self.peek_byte() {
                        Some(b'=') => {
                            self.eat_byte();
                            self.token(t!("==="))
                        }
                        _ => self.token(t!("==")),
                    }
                }
                _ => self.token(t!("=")),
            },
            b'!' => match self.peek_byte() {
                Some(b'=') => {
                    self.eat_byte();
                    match self.peek_byte() {
                        Some(b'=') => {
                            self.eat_byte();
                            self.token(t!("!=="))
                        }
                        _ => self.token(t!("!=")),
                    }
                }
                _ => self.token(t!("!")),
            },
            b'&' => match self.peek_byte() {
                Some(b'=') => {
                    self.eat_byte();
                    self.token(t!("&="))
                }
                Some(b'&') => {
                    self.eat_byte();
                    self.token(t!("&&"))
                }
                _ => self.token(t!("&")),
            },
            b'|' => match self.peek_byte() {
                Some(b'=') => {
                    self.eat_byte();
                    self.token(t!("|="))
                }
                Some(b'|') => {
                    self.eat_byte();
                    self.token(t!("||"))
                }
                _ => self.token(t!("|")),
            },
            b'^' => match self.peek_byte() {
                Some(b'=') => {
                    self.eat_byte();
                    self.token(t!("^="))
                }
                _ => self.token(t!("^")),
            },
            b'.' => match self.peek_byte() {
                Some(b'.') => {
                    self.eat_byte();
                    match self.peek_byte() {
                        Some(b'.') => {
                            self.eat_byte();
                            self.token(t!("..."))
                        }
                        _ => self.token(t!("..")),
                    }
                }
                Some(x) if x.is_ascii_digit() => self.lex_number(b'.')?,
                _ => self.token(t!(".")),
            },
            b'$' => self.lex_ident(self.offset - 1)?,
            b'_' => self.lex_ident(self.offset - 1)?,
            b'\\' => todo!(),
            b'\'' => self.lex_string(b'\'')?,
            b'\"' => self.lex_string(b'\"')?,

            x if x.is_ascii_digit() => self.lex_number(x)?,
            x if x.is_ascii_alphabetic() => self.lex_ident(self.offset - 1)?,
            x if !x.is_ascii() => return self.match_char(x),
            _ => return Err(ErrorKind::InvalidToken),
        };

        Ok(Some(token))
    }

    fn eat_byte(&mut self) {
        self.offset += 1;
    }

    fn next_byte(&mut self) -> Option<u8> {
        let byte = *self.source.source().as_bytes().get(self.offset)?;
        self.offset += 1;
        Some(byte)
    }

    fn peek_byte(&mut self) -> Option<u8> {
        self.source.source().as_bytes().get(self.offset).copied()
    }
}
