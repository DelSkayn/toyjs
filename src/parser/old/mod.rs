use crate::{
    ast::*,
    lexer::Lexer,
    source::Source,
    token::{DelimToken, Kw, Span, Token, TokenKind},
};
use std::collections::VecDeque;

#[macro_use]
mod macros;

mod error;
mod expr;
pub use error::{ParseError, ParseErrorKind};
mod lit;
mod stmt;

type PResult<'a, T> = Result<T, ParseError<'a>>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    peeked_token: Option<Token<'a>>,
    prev_span: Span,
    strict_mode: bool,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer,
            prev_span: Span { lo: 0, hi: 0 },
            peeked_token: None,
            strict_mode: false,
        }
    }

    pub fn is(&mut self, kind: TokenKind) -> bool {
        let token = self.peek();
        token.map(|x| x.kind == kind).unwrap_or(false)
    }

    pub fn eat(&mut self, kind: TokenKind) -> bool {
        if self.is(kind) {
            self.next();
            return true;
        }
        return false;
    }

    pub fn source(&self) -> Source<'a> {
        self.lexer.src.clone()
    }

    pub fn strict(&mut self, enabled: bool) -> &mut Self {
        self.strict_mode = enabled;
        self
    }

    pub fn cur_span(&mut self) -> Span {
        self.peek().map(|e| e.span).unwrap_or(self.prev_span)
    }

    pub fn parse_script(&mut self) -> PResult<'a, Script<'a>> {
        let mut script = Script { stmts: Vec::new() };
        while self.peek().is_some() {
            script.stmts.push(self.parse_stmt()?);
            self.eat(TokenKind::SemiColon);
        }
        Ok(script)
    }

    pub fn next_with_lt(&mut self) -> Option<Token<'a>> {
        let res = if let Some(x) = self.peeked_token.take() {
            Some(x)
        } else {
            self.lexer.next()
        };
        res.map(|e| self.prev_span = e.span);
        res
    }

    pub fn peek_with_lt(&mut self) -> Option<Token<'a>> {
        let res = if let Some(x) = self.peeked_token {
            Some(x)
        } else {
            let t = self.lexer.next();
            t.map(|t| {
                self.peeked_token = Some(t);
                t
            })
        };
        res
    }

    pub fn next(&mut self) -> Option<Token<'a>> {
        let mut res = self.next_with_lt();
        while let Some(tok!("\n")) = res.map(|e| e.kind) {
            res = self.next_with_lt();
        }
        res
    }

    pub fn peek(&mut self) -> Option<Token<'a>> {
        let mut res = self.peek_with_lt();
        while let Some(tok!("\n")) = res.map(|e| e.kind) {
            self.next_with_lt();
            res = self.peek_with_lt();
        }
        res
    }

    pub fn is_lit(&mut self) -> bool {
        let t = self.peek();
        if t.is_none() {
            return false;
        }
        let t = t.unwrap();
        match t.kind {
            TokenKind::Lit(_) => true,
            _ => false,
        }
    }

    // TODO make this faster
    pub fn is_ident(&mut self) -> bool {
        self.peek()
            .map(|e| match e.kind {
                TokenKind::Ident(_) => true,
                _ => false,
            })
            .unwrap_or(false)
    }
}
