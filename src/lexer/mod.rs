use crate::{
    interner::Interner,
    source::Span,
    token::{LitToken, Token, TokenKind},
};
use std::{char, result::Result as StdResult};

pub type Result<T> = StdResult<T, LexerError>;

mod chars;
mod ident;
mod utf;

pub enum LexerErrorKind {
    InvalidUtf8,
    UnknownToken,
    UnClosedMultiLineComment,
    UnClosedString,
}

pub struct LexerError {
    pub span: Span,
    pub kind: LexerErrorKind,
}

pub struct Lexer<'a> {
    pub interner: &'a mut Interner,
    bytes: &'a [u8],
    last_end: usize,
    cur: usize,
    line: u64,
    column: u64,
    error: bool,
    buffer: String,
    peek: Option<(char, u32)>,
}

impl<'a> Lexer<'a> {
    pub fn new(bytes: &'a [u8], interner: &'a mut Interner) -> Self {
        Lexer {
            interner,
            bytes,
            last_end: 0,
            cur: 0,
            line: 0,
            column: 0,
            error: false,
            buffer: String::new(),
            peek: None,
        }
    }

    fn eat(&mut self) -> Option<u8> {
        if self.bytes.len() < self.cur {
            self.peek = None;
            let res = self.bytes[self.cur];
            self.cur += 1;
            Some(res)
        } else {
            None
        }
    }

    fn eat_skip_whitespace(&mut self) -> Result<Option<u8>> {
        loop {
            let peek = if let Some(x) = self.peek() {
                x
            } else {
                return Ok(None);
            };
            match peek {
                chars::TAB
                | chars::VT
                | chars::FF
                | chars::SP
                | chars::NBSP
                | chars::LF
                | chars::CR => self.last_end = self.cur,
                x if Self::is_non_ascii(x) => match self.peek_char()?.unwrap() {
                    // These are all the unicode whitespace characters
                    chars::LS
                    | chars::PS
                    | chars::ZWNBSP
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
                    | chars::USP_15 => self.last_end = self.cur,
                    _ => {
                        return Ok(self.eat());
                    }
                },
                _ => return Ok(self.eat()),
            }
        }
    }

    fn peek(&self) -> Option<u8> {
        if self.bytes.len() < self.cur + 1 {
            Some(self.bytes[self.cur + 1])
        } else {
            None
        }
    }

    fn error(&mut self, error: LexerErrorKind) -> LexerError {
        let cur = self.cur;
        let start = self.last_end;
        self.last_end = self.cur;
        assert!(
            cur <= u32::MAX as usize,
            "span index larger then u32::MAX, source file possibly to big to handle!"
        );
        assert!(
            start <= u32::MAX as usize,
            "span index larger then u32::MAX, source file possibly to big to handle!"
        );
        let span = Span {
            begin: start as u32,
            end: cur as u32,
        };
        LexerError { span, kind: error }
    }

    fn token(&mut self, kind: TokenKind) -> Result<Option<Token>> {
        let cur = self.cur;
        let start = self.last_end;
        self.last_end = self.cur;
        assert!(
            cur <= u32::MAX as usize,
            "span index larger then u32::MAX, source file possibly to big to handle!"
        );
        assert!(
            start <= u32::MAX as usize,
            "span index larger then u32::MAX, source file possibly to big to handle!"
        );
        let span = Span {
            begin: start as u32,
            end: cur as u32,
        };
        Ok(Some(Token { span, kind }))
    }

    fn eat_line_comment(&mut self) -> Result<()> {
        loop {
            let peek = if let Some(x) = self.peek() {
                x
            } else {
                break;
            };
            match peek {
                chars::LF => {
                    self.eat();
                    break;
                }
                chars::CR => {
                    self.eat();
                    if self.peek() == Some(chars::LF) {
                        self.eat();
                    }
                    break;
                }
                x if Self::is_non_ascii(x) => match self.eat_char()?.unwrap() {
                    chars::LS => break,
                    chars::PS => break,
                    _ => {}
                },
                _ => {}
            }
        }
        Ok(())
    }

    fn eat_multi_line_comment(&mut self) -> Result<()> {
        loop {
            match self
                .eat()
                .ok_or_else(|| self.error(LexerErrorKind::UnClosedMultiLineComment))?
            {
                b'*' => {
                    match self
                        .eat()
                        .ok_or_else(|| self.error(LexerErrorKind::UnClosedMultiLineComment))?
                    {
                        b'/' => return Ok(()),
                        _ => {}
                    }
                }
                _ => {}
            }
        }
    }

    fn lex_string(&mut self, start: u8) -> Result<Option<Token>> {
        self.buffer.clear();
        loop {
            match self
                .eat()
                .ok_or_else(|| self.error(LexerErrorKind::UnClosedString))?
            {
                chars::LF | chars::CR => return Err(self.error(LexerErrorKind::UnClosedString)),
                s if s == start => {
                    let s = self.interner.intern(&self.buffer);
                    return self.token(TokenKind::Lit(LitToken::String(s)));
                }
                x if Self::is_non_ascii(x) => match self.eat_char()?.unwrap() {
                    chars::LS | chars::PS => return Err(self.error(LexerErrorKind::UnClosedString)),
                    x => self.buffer.push(x),
                },
                x => self.buffer.push(x.into()),
            }
        }
    }

    pub fn next(&mut self) -> Result<Option<Token>> {
        let next = self.eat_skip_whitespace()?;
        if next.is_none() {
            return Ok(None);
        }
        let next = next.unwrap();
        match next {
            b';' => self.token(t!(";")),
            b',' => self.token(t!(",")),
            b'(' => self.token(t!("(")),
            b')' => self.token(t!(")")),
            b'{' => self.token(t!("{")),
            b'}' => self.token(t!("}")),
            b'[' => self.token(t!("[")),
            b']' => self.token(t!("]")),
            b'+' => match self.peek() {
                Some(b'=') => {
                    self.eat();
                    self.token(t!("+="))
                }
                Some(b'+') => {
                    self.eat();
                    self.token(t!("++"))
                }
                _ => self.token(t!("+")),
            },
            b'-' => match self.peek() {
                Some(b'=') => {
                    self.eat();
                    self.token(t!("-="))
                }
                Some(b'-') => {
                    self.eat();
                    self.token(t!("--"))
                }
                _ => self.token(t!("-")),
            },

            b'*' => match self.peek() {
                Some(b'=') => {
                    self.eat();
                    self.token(t!("*"))
                }
                Some(b'*') => {
                    self.eat();
                    match self.peek() {
                        Some(b'=') => {
                            self.eat();
                            self.token(t!("**="))
                        }
                        _ => self.token(t!("**")),
                    }
                }
                _ => self.token(t!("*")),
            },

            b'/' => match self.peek() {
                Some(b'=') => {
                    self.eat();
                    self.token(t!("/="))
                }
                Some(b'/') => {
                    self.eat_line_comment()?;
                    self.next()
                }
                Some(b'*') => {
                    self.eat_multi_line_comment()?;
                    self.next()
                }
                _ => self.token(t!("/")),
            },
            b'~' => self.token(t!("~")),
            b'%' => match self.peek() {
                Some(b'=') => {
                    self.eat();
                    self.token(t!("%="))
                }
                _ => self.token(t!("%")),
            },
            b'<' => match self.peek() {
                Some(b'=') => {
                    self.eat();
                    self.token(t!("<="))
                }
                Some(b'<') => {
                    self.eat();
                    match self.peek() {
                        Some(b'=') => {
                            self.eat();
                            self.token(t!("<<="))
                        }
                        _ => self.token(t!("<<")),
                    }
                }
                _ => self.token(t!("<")),
            },
            b'>' => match self.peek() {
                Some(b'=') => {
                    self.eat();
                    self.token(t!(">="))
                }
                Some(b'>') => {
                    self.eat();
                    match self.peek() {
                        Some(b'=') => {
                            self.eat();
                            self.token(t!(">>="))
                        }
                        Some(b'>') => {
                            self.eat();
                            match self.peek() {
                                Some(b'=') => {
                                    self.eat();
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
            b'=' => match self.peek() {
                Some(b'=') => {
                    self.eat();
                    match self.peek() {
                        Some(b'=') => {
                            self.eat();
                            self.token(t!("==="))
                        }
                        _ => self.token(t!("==")),
                    }
                }
                _ => self.token(TokenKind::Assign),
            },
            b'!' => match self.peek() {
                Some(b'=') => {
                    self.eat();
                    match self.peek() {
                        Some(b'=') => {
                            self.eat();
                            self.token(t!("!=="))
                        }
                        _ => self.token(t!("!=")),
                    }
                }
                _ => self.token(t!("!")),
            },
            b'&' => match self.peek() {
                Some(b'=') => {
                    self.eat();
                    self.token(t!("&="))
                }
                Some(b'&') => {
                    self.eat();
                    self.token(t!("&&"))
                }
                _ => self.token(t!("&")),
            },
            b'|' => match self.peek() {
                Some(b'=') => {
                    self.eat();
                    self.token(t!("|="))
                }
                Some(b'|') => {
                    self.eat();
                    self.token(t!("||"))
                }
                _ => self.token(t!("|")),
            },
            b'^' => match self.peek() {
                Some(b'=') => {
                    self.eat();
                    self.token(t!("^="))
                }
                _ => self.token(t!("^")),
            },
            b':' => match self.peek() {
                Some(b':') => {
                    self.eat();
                    self.token(t!("::"))
                }
                _ => self.token(t!(":")),
            },
            b'.' => match self.peek() {
                Some(b'.') => {
                    self.eat();
                    match self.peek() {
                        Some(b'.') => {
                            self.eat();
                            self.token(t!("..."))
                        }
                        _ => self.token(t!("..")),
                    }
                }
                Some(_) => todo!(), //if x.is_digit(10) => self.parse_number(b'.'),
                _ => self.token(TokenKind::Dot),
            },
            b'"' => self.lex_string(b'"'),
            b'\'' => self.lex_string(b'\''),
            _ => {
                self.cur -= 1;
                self.lex_ident()
            }
        }
    }
}
