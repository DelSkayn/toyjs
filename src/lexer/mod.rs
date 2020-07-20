use crate::{
    interner::Interner,
    source::Span,
    token::{LitToken, Token, TokenKind},
};
use std::{char, result::Result as StdResult};

pub type Result<T> = StdResult<T, LexerError>;

mod chars;
mod ident;
mod number;
mod string;
mod utf;

#[derive(Debug)]
pub enum LexerErrorKind {
    InvalidUtf8,
    UnknownToken,
    UnClosedMultiLineComment,
    UnClosedString,
    InvalidNumber,
    InvalidEscapeCode,
    NotYetSupported,
}

#[derive(Debug)]
pub struct LexerError {
    pub span: Span,
    pub kind: LexerErrorKind,
}

pub struct Lexer<'a> {
    pub interner: &'a mut Interner,
    bytes: &'a [u8],

    pending_char: Option<(char, u8)>,

    last: usize,
    cur: usize,

    buffer: String,
}

impl<'a> Lexer<'a> {
    pub fn new(bytes: &'a [u8], interner: &'a mut Interner) -> Self {
        Lexer {
            interner,
            bytes,
            last: 0,
            cur: 0,
            buffer: String::new(),
            pending_char: None,
        }
    }

    /// Returns the next byte and consumes it.
    fn next_byte(&mut self) -> Option<u8> {
        if self.cur < self.bytes.len() {
            self.pending_char = None;
            let res = self.bytes[self.cur];
            self.cur += 1;
            Some(res)
        } else {
            None
        }
    }

    /// Returns the next byte
    fn peek_byte(&self) -> Option<u8> {
        if self.cur < self.bytes.len() {
            Some(self.bytes[self.cur])
        } else {
            None
        }
    }

    /// Returns the next byte skipping any whitespace characters.
    /// Will error on invalid utf-8 characters.
    fn next_skip_whitespace(&mut self) -> Result<Option<u8>> {
        loop {
            let peek = if let Some(x) = self.peek_byte() {
                x
            } else {
                return Ok(None);
            };
            match peek {
                chars::TAB | chars::VT | chars::FF | chars::SP | chars::NBSP => {
                    self.next_byte();
                    self.last = self.cur;
                }
                x if Self::is_non_ascii(x) => match self.peek_char()?.unwrap() {
                    // These are all the unicode whitespace characters
                    chars::ZWNBSP
                    | chars::USP_1
                    | chars::USP_2
                    | chars::USP_3
                    | chars::USP_4
                    | chars::USP_5
                    | chars::USP_6
                    | chars::USP_7
                    | chars::USP_8
                    | chars::USP_9
                    | chars::USP_10
                    | chars::USP_11
                    | chars::USP_12
                    | chars::USP_13
                    | chars::USP_14
                    | chars::USP_15 => {
                        self.last = self.cur;
                        self.next_byte();
                    }
                    _ => {
                        return Ok(self.next_byte());
                    }
                },
                _ => return Ok(self.next_byte()),
            }
        }
    }

    /// Will eat the next line terminator if there is one
    /// returns true if a line terminator was eaten.
    fn eat_line_terminator(&mut self, c: u8) -> bool {
        match c {
            chars::LF => {
                self.next_byte();
                return true;
            }
            chars::CR => {
                self.next_byte();
                if self.peek_byte() == Some(chars::LF) {
                    self.next_byte();
                }
                return true;
            }
            x if Self::is_non_ascii(x) => {
                self.cur -= 1;
                match self.peek_char() {
                    Ok(Some(chars::LS)) | Ok(Some(chars::PS)) => {
                        self.next_char().unwrap();
                        return true;
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        return false;
    }

    fn error(&mut self, error: LexerErrorKind) -> LexerError {
        let cur = self.cur;
        let start = self.last;
        self.last = self.cur;
        assert!(
            cur <= u32::MAX as usize,
            "span index larger then u32::MAX, source file possibly to big to handle!"
        );
        assert!(
            start <= u32::MAX as usize,
            "span index larger then u32::MAX, source file possibly to big to handle!"
        );
        let span = Span {
            lo: start as u32,
            hi: cur as u32,
        };
        LexerError { span, kind: error }
    }

    fn token(&mut self, kind: TokenKind) -> Result<Option<Token>> {
        let cur = self.cur;
        let start = self.last;
        self.last = self.cur;
        assert!(
            cur <= u32::MAX as usize,
            "span index larger then u32::MAX, source file possibly to big to handle!"
        );
        assert!(
            start <= u32::MAX as usize,
            "span index larger then u32::MAX, source file possibly to big to handle!"
        );
        let span = Span {
            lo: start as u32,
            hi: cur as u32,
        };
        Ok(Some(Token { span, kind }))
    }

    fn lex_line_comment(&mut self) -> Result<()> {
        loop {
            let next = if let Some(x) = self.next_byte() {
                x
            } else {
                break;
            };
            if self.eat_line_terminator(next) {
                break;
            }
            self.next_byte();
        }
        Ok(())
    }

    fn lex_multi_line_comment(&mut self) -> Result<()> {
        loop {
            match self
                .next_byte()
                .ok_or_else(|| self.error(LexerErrorKind::UnClosedMultiLineComment))?
            {
                b'*' => match self.peek_byte() {
                    Some(b'/') => {
                        self.next_byte();
                        return Ok(());
                    }
                    _ => {}
                },
                _ => {}
            }
        }
    }

    /// Returns the next token from the source if there is one.
    pub fn next(&mut self) -> Result<Option<Token>> {
        let next = self.next_skip_whitespace()?;
        if next.is_none() {
            return Ok(None);
        }
        let next = next.unwrap();
        match next {
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
                    self.next_byte();
                    self.token(t!("??"))
                }
                Some(b'.') => {
                    self.next_byte();
                    if self.peek_byte().map(Self::is_digit).unwrap_or(false) {
                        self.cur -= 1;
                        self.token(t!("?"))
                    } else {
                        self.token(t!("?."))
                    }
                }
                _ => self.token(t!("?")),
            },
            b'+' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_byte();
                    self.token(t!("+="))
                }
                Some(b'+') => {
                    self.next_byte();
                    self.token(t!("++"))
                }
                _ => self.token(t!("+")),
            },
            b'-' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_byte();
                    self.token(t!("-="))
                }
                Some(b'-') => {
                    self.next_byte();
                    self.token(t!("--"))
                }
                _ => self.token(t!("-")),
            },

            b'*' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_byte();
                    self.token(t!("*"))
                }
                Some(b'*') => {
                    self.next_byte();
                    match self.peek_byte() {
                        Some(b'=') => {
                            self.next_byte();
                            self.token(t!("**="))
                        }
                        _ => self.token(t!("**")),
                    }
                }
                _ => self.token(t!("*")),
            },

            b'/' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_byte();
                    self.token(t!("/="))
                }
                Some(b'/') => {
                    self.lex_line_comment()?;
                    self.next()
                }
                Some(b'*') => {
                    self.lex_multi_line_comment()?;
                    self.next()
                }
                _ => self.token(t!("/")),
            },
            b'~' => self.token(t!("~")),
            b'%' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_byte();
                    self.token(t!("%="))
                }
                _ => self.token(t!("%")),
            },
            b'<' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_byte();
                    self.token(t!("<="))
                }
                Some(b'<') => {
                    self.next_byte();
                    match self.peek_byte() {
                        Some(b'=') => {
                            self.next_byte();
                            self.token(t!("<<="))
                        }
                        _ => self.token(t!("<<")),
                    }
                }
                _ => self.token(t!("<")),
            },
            b'>' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_byte();
                    self.token(t!(">="))
                }
                Some(b'>') => {
                    self.next_byte();
                    match self.peek_byte() {
                        Some(b'=') => {
                            self.next_byte();
                            self.token(t!(">>="))
                        }
                        Some(b'>') => {
                            self.next_byte();
                            match self.peek_byte() {
                                Some(b'=') => {
                                    self.next_byte();
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
                    self.next_byte();
                    match self.peek_byte() {
                        Some(b'=') => {
                            self.next_byte();
                            self.token(t!("==="))
                        }
                        _ => self.token(t!("==")),
                    }
                }
                _ => self.token(t!("=")),
            },
            b'!' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_byte();
                    match self.peek_byte() {
                        Some(b'=') => {
                            self.next_byte();
                            self.token(t!("!=="))
                        }
                        _ => self.token(t!("!=")),
                    }
                }
                _ => self.token(t!("!")),
            },
            b'&' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_byte();
                    self.token(t!("&="))
                }
                Some(b'&') => {
                    self.next_byte();
                    self.token(t!("&&"))
                }
                _ => self.token(t!("&")),
            },
            b'|' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_byte();
                    self.token(t!("|="))
                }
                Some(b'|') => {
                    self.next_byte();
                    self.token(t!("||"))
                }
                _ => self.token(t!("|")),
            },
            b'^' => match self.peek_byte() {
                Some(b'=') => {
                    self.next_byte();
                    self.token(t!("^="))
                }
                _ => self.token(t!("^")),
            },
            b'.' => match self.peek_byte() {
                Some(b'.') => {
                    self.next_byte();
                    match self.peek_byte() {
                        Some(b'.') => {
                            self.next_byte();
                            self.token(t!("..."))
                        }
                        _ => self.token(t!("..")),
                    }
                }
                Some(x) if Self::is_digit(x) => self.lex_number(b'.'),
                _ => self.token(TokenKind::Dot),
            },
            x if self.eat_line_terminator(x) => self.token(t!("\n")),
            b'`' => return Err(self.error(LexerErrorKind::NotYetSupported)),
            b'"' => self.lex_string(b'"'),
            b'\'' => self.lex_string(b'\''),
            x if Self::is_digit(x) => self.lex_number(x),
            _ => self.lex_ident(),
        }
    }
}
