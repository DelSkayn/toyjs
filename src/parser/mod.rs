use crate::{
    ast::*,
    lexer::Lexer,
    source::{Source, Span},
    token::{Token, TokenKind},
};

#[macro_use]
mod macros;

mod error;
pub use error::{ParseError, ParseErrorKind};

mod expr;
mod stmt;

type PResult<'a, T> = Result<T, ParseError<'a>>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    peek: Option<Token<'a>>,
    pref_span: Span,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer,
            peek: None,
            pref_span: Span { lo: 0, hi: 0 },
        }
    }

    pub fn peek_with_lt(&mut self) -> Option<Token<'a>> {
        match self.peek {
            Some(x) => Some(x),
            None => {
                self.peek = self.lexer.next();
                self.peek
            }
        }
    }

    pub fn next_with_lt(&mut self) -> Option<Token<'a>> {
        self.peek.take().or_else(|| {
            let l = self.lexer.next();
            l.map(|e| self.pref_span = e.span);
            l
        })
    }

    pub fn eat_with_lt(&mut self, kind: TokenKind) -> bool {
        if self.peek_with_lt().filter(|e| e.kind == kind).is_some() {
            self.next_with_lt();
            true
        } else {
            false
        }
    }

    pub fn peek(&mut self) -> Option<Token<'a>> {
        while let Some(x) = self.peek_with_lt() {
            if x.kind != tok!("\n") {
                return Some(x);
            }
            self.next_with_lt();
        }
        None
    }

    pub fn next(&mut self) -> Option<Token<'a>> {
        while let Some(x) = self.next_with_lt() {
            if x.kind != tok!("\n") {
                return Some(x);
            }
        }
        None
    }

    pub fn eat(&mut self, kind: TokenKind) -> bool {
        if let Some(x) = self.peek() {
            if x.kind == kind {
                self.next();
                return true;
            }
        }
        false
    }

    pub fn source(&self) -> Source<'a> {
        self.lexer.src.clone()
    }

    pub fn cur_span(&mut self) -> Span {
        if let Some(x) = self.peek() {
            return x.span;
        }
        return self.pref_span;
    }

    pub fn cur_string(&mut self) -> &'a str {
        self.source().str(self.cur_span())
    }

    /// Parse a js script.
    /// One of the 2 entry points into parsing
    pub fn parse_script(&mut self) -> PResult<'a, Script<'a>> {
        trace!("parse: script");
        let mut stmts = Vec::new();
        while self.peek().is_some() {
            stmts.push(self.parse_stmt()?);
            eat!(self, ";");
        }
        Ok(Script { stmts })
    }

    /// Parse a js module.
    /// One of the 2 entry points into parsing
    pub fn parse_module(&mut self) -> PResult<'a, Script<'a>> {
        trace!("parse: module");
        to_do!(self)
    }
}
