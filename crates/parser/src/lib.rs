#![allow(dead_code)]

use ast::Variables;
use bumpalo::{collections::Vec, Bump};
use common::{
    interner::Interner,
    source::{Source, Span},
};
use lexer::Lexer;
use token::{t, Token, TokenKind};

use std::result::Result as StdResult;

#[macro_use]
mod macros;

mod error;
pub use error::{Error, ErrorKind};

mod expr;
mod prime;
mod stmt;

pub type Result<T> = StdResult<T, Error>;

/// The toyjs parser.
pub struct Parser<'source, 'alloc> {
    lexer: Lexer<'source>,
    peek: Option<Token>,
    bump: &'alloc Bump,
    last_span: Span,
    variables: &'source mut Variables<'alloc>,
}

impl<'source, 'alloc> Parser<'source, 'alloc> {
    pub fn from_lexer(
        lexer: Lexer<'source>,
        bump: &'alloc Bump,
        variables: &'source mut Variables<'alloc>,
    ) -> Self {
        Parser {
            lexer,
            peek: None,
            bump,
            last_span: Span { low: 0, hi: 0 },
            variables,
        }
    }

    pub fn from_source(
        source: &'source Source,
        interner: &'source mut Interner,
        bump: &'alloc Bump,
        variables: &'source mut Variables<'alloc>,
    ) -> Self {
        Parser {
            lexer: Lexer::new(source, interner),
            peek: None,
            bump,
            last_span: Span { low: 0, hi: 0 },
            variables,
        }
    }
    fn next(&mut self) -> Result<Option<Token>> {
        loop {
            match self.next_lt()? {
                Some(x) if x.kind == t!("\n") => continue,
                x => return Ok(x),
            }
        }
    }

    fn next_lt(&mut self) -> Result<Option<Token>> {
        if let Some(x) = self.peek.take() {
            Ok(Some(x))
        } else {
            Ok(self.lexer.next()?.map(|e| {
                self.last_span = e.span;
                e
            }))
        }
    }

    fn peek_lt(&mut self) -> Result<Option<Token>> {
        if let Some(x) = self.peek {
            Ok(Some(x))
        } else {
            let next = self.lexer.next()?.map(|e| {
                self.last_span = e.span;
                e
            });
            self.peek = next;
            Ok(next)
        }
    }

    fn peek(&mut self) -> Result<Option<Token>> {
        if let Some(x) = self.peek {
            if x.kind != t!("\n") {
                return Ok(Some(x));
            }
        }
        self.peek = None;
        loop {
            match self.lexer.next()? {
                None => return Ok(None),
                Some(x) if x.kind != t!("\n") => {
                    self.last_span = x.span;
                    self.peek = Some(x);
                    return Ok(Some(x));
                }
                _ => {}
            }
        }
    }

    /// Short hand for `peek().map(|e| e.kind)`
    fn peek_kind(&mut self) -> Result<Option<TokenKind>> {
        Ok(self.peek()?.map(|e| e.kind))
    }

    /// Consume the next token if it is equal to the given kind
    fn eat(&mut self, kind: TokenKind) -> Result<bool> {
        match self.peek()? {
            None => Ok(false),
            Some(x) => {
                if x.kind == kind {
                    self.peek = None;
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
        }
    }

    pub fn parse_script(mut self) -> Result<ast::Script<'alloc>> {
        let mut stmts = Vec::new_in(self.bump);
        while self.peek()?.is_some() {
            stmts.push(self.parse_stmt()?);
        }
        Ok(ast::Script(stmts))
    }

    pub fn parse_module(&mut self) -> Result<()> {
        todo!();
    }
}
