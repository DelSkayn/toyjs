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
        if self.cur < self.bytes.len() {
            self.peek = None;
            let res = self.bytes[self.cur];
            self.cur += 1;
            Some(res)
        } else {
            None
        }
    }

    fn peek(&self) -> Option<u8> {
        if self.cur < self.bytes.len() {
            Some(self.bytes[self.cur])
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
                chars::TAB | chars::VT | chars::FF | chars::SP | chars::NBSP => {
                    self.last_end = self.cur;
                    self.eat();
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
                        self.last_end = self.cur;
                        self.eat();
                    }
                    _ => {
                        return Ok(self.eat());
                    }
                },
                _ => return Ok(self.eat()),
            }
        }
    }

    fn eat_newline(&mut self, c: u8) -> bool {
        match c {
            chars::LF => {
                self.eat();
                return true;
            }
            chars::CR => {
                self.eat();
                if self.peek() == Some(chars::LF) {
                    self.eat();
                }
                return true;
            }
            x if Self::is_non_ascii(x) => {
                self.cur -= 1;
                match self.peek_char() {
                    Ok(Some(chars::LS)) | Ok(Some(chars::PS)) => {
                        self.eat_char().unwrap();
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
            lo: start as u32,
            hi: cur as u32,
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
            lo: start as u32,
            hi: cur as u32,
        };
        Ok(Some(Token { span, kind }))
    }

    fn eat_line_comment(&mut self) -> Result<()> {
        loop {
            let next = if let Some(x) = self.eat() {
                x
            } else {
                break;
            };
            if self.eat_newline(next) {
                break;
            }
            self.eat();
        }
        Ok(())
    }

    fn eat_multi_line_comment(&mut self) -> Result<()> {
        loop {
            match self
                .eat()
                .ok_or_else(|| self.error(LexerErrorKind::UnClosedMultiLineComment))?
            {
                b'*' => match self.peek() {
                    Some(b'/') => {
                        self.eat();
                        return Ok(());
                    }
                    _ => {}
                },
                _ => {}
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
                Some(x) if Self::is_digit(x) => self.lex_number(b'.'),
                _ => self.token(TokenKind::Dot),
            },
            x if self.eat_newline(x) => self.token(t!("\n")),
            b'`' => return Err(self.error(LexerErrorKind::NotYetSupported)),
            b'"' => self.lex_string(b'"'),
            b'\'' => self.lex_string(b'\''),
            x if Self::is_digit(x) => self.lex_number(x),
            _ => self.lex_ident(),
        }
    }
}
