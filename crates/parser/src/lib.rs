#![allow(dead_code)]

use std::any::Any;

use ast::{Ast, AstStorage, NodeId};
use common::span::Span;
use lexer::{Lexer, State};
use token::{t, Token, TokenKind};

mod macros;

mod constants;
mod error;
mod expr;
mod prime;
mod stmt;

pub use error::Error;
pub type Result<T> = std::result::Result<T, Error>;

pub struct Parser<'a> {
    pub lexer: Lexer<'a>,
    peek: Option<Token>,
    pub ast: Ast,
    last_span: Span,
    state: ParserState,
}

#[derive(Debug)]
pub struct ParserState {
    eat_line_terminator: bool,
    strict: bool,
    yield_ident: bool,
    await_ident: bool,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer,
            ast: Ast::new(AstStorage::default()),
            peek: None,
            last_span: Span::empty(),
            state: ParserState {
                eat_line_terminator: true,
                strict: false,
                yield_ident: true,
                await_ident: true,
            },
        }
    }

    fn last_span(&self) -> &Span {
        &self.last_span
    }

    fn with_lexer_state<R, F: FnOnce(&mut Self) -> R>(&mut self, state: State, f: F) -> R {
        self.lexer.push_state(state);
        let res = f(self);
        self.lexer.pop_state();
        res
    }

    fn retrieve_next_token(&mut self) -> Option<Token> {
        while let Some(x) = self.lexer.next_token() {
            match x.kind_and_data.kind() {
                t!(" ") => {}
                t!("\n") if self.state.eat_line_terminator => {}
                _ => return Some(x),
            }
        }
        None
    }

    /// Return the next token
    fn next(&mut self) -> Option<Token> {
        let res = self.peek.take().or_else(|| self.retrieve_next_token());
        if let Some(x) = res.as_ref() {
            self.last_span = x.span.clone();
        }
        res
    }

    fn following_span(&mut self) -> Span {
        if let Some(x) = self.peek.as_ref() {
            return x.span.clone();
        }
        self.peek = self.retrieve_next_token();
        self.peek
            .as_ref()
            .map(|x| x.span.clone())
            .unwrap_or_else(|| self.last_span.clone())
    }

    /// Returns the next token in the list without advancing the token iterator..
    fn peek(&mut self) -> Option<Token> {
        if let Some(x) = self.peek.as_ref() {
            return Some(x.clone());
        }
        self.peek = self.retrieve_next_token();
        self.peek.clone()
    }

    /// Returns the next token in the list without advancing the token iterator..
    fn peek_kind(&mut self) -> Option<TokenKind> {
        if let Some(x) = self.peek.as_ref().map(|x| x.kind_and_data.kind()) {
            Some(x)
        } else {
            self.peek = self.retrieve_next_token();
            self.peek.as_ref().map(|x| x.kind_and_data.kind())
        }
    }

    /// Eat a token if it is of a specific type.
    /// Returns true if a token was eaten.
    fn eat(&mut self, which: TokenKind) -> bool {
        if self.peek().map(|x| x.kind_and_data.kind()) == Some(which) {
            self.peek = None;
            true
        } else {
            false
        }
    }

    pub fn push<N: Any>(&mut self, node: N) -> NodeId<N> {
        self.ast.push_node(node)
    }
}
