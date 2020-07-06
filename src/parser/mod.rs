use crate::{
    ast::*,
    lexer::Lexer,
    source::{Source, Span},
    token::{DelimToken, Token, TokenKind},
};

#[macro_use]
mod macros;

mod error;
pub use error::{ParseError, ParseErrorKind};

mod class;
mod decl;
mod expr;
mod prime;
mod stmt;

type PResult<'a, T> = Result<T, ParseError<'a>>;

#[derive(Clone, Copy)]
pub struct StateFlags {
    pub _in: bool,
    pub _yield: bool,
    pub _await: bool,
    pub _break: bool,
    pub _continue: bool,
    pub _return: bool,
}

impl Default for StateFlags {
    fn default() -> Self {
        StateFlags {
            _in: true,
            _yield: false,
            _await: false,
            _break: false,
            _continue: false,
            _return: false,
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    peek: Option<Token<'a>>,
    pref_span: Span,
    state: StateFlags,
    //parens: Vec<(DelimToken, Span)>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer,
            peek: None,
            pref_span: Span { lo: 0, hi: 0 },
            state: StateFlags::default(),
            //parens: Vec::new(),
        }
    }

    #[must_use]
    pub fn alter_state<T>(
        &mut self,
        alter: impl FnOnce(&mut StateFlags),
        this: impl FnOnce(&mut Parser<'a>) -> T,
    ) -> T {
        let old = self.state;
        alter(&mut self.state);
        let res = this(self);
        self.state = old;
        res
    }

    pub fn is_lt(&mut self) -> bool {
        self.peek_with_lt()
            .map(|e| e.kind == tok!("\n"))
            .unwrap_or(false)
    }

    pub fn peek_with_lt(&mut self) -> Option<Token<'a>> {
        let res = match self.peek {
            Some(x) => Some(x),
            None => {
                self.peek = self.lexer.next();
                self.peek
            }
        };
        trace!("peek {:?}", res);
        res
    }

    pub fn next_with_lt(&mut self) -> Option<Token<'a>> {
        let res = self.peek.take().or_else(|| {
            let l = self.lexer.next();
            l.map(|e| self.pref_span = e.span);
            l
        });
        trace!("next {:?}", res);
        res
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

    pub fn peek_kind(&mut self) -> Option<TokenKind<'a>> {
        self.peek().map(|t| t.kind)
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

    pub fn next_ident(&mut self) -> Option<Ident> {
        match self.peek_kind() {
            Some(TokenKind::Ident(x)) => {
                self.next();
                Some(Ident(x.to_string()))
            }
            _ => None,
        }
    }

    pub fn cur_span(&mut self) -> Span {
        if let Some(x) = self.peek() {
            return x.span;
        }
        return self.pref_span;
    }

    /// Parse a js script.
    /// One of the 2 entry points into parsing
    pub fn parse_script(&mut self) -> PResult<'a, Script> {
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
    pub fn parse_module(&mut self) -> PResult<'a, Script> {
        trace!("parse: module");
        to_do!(self)
    }
}
