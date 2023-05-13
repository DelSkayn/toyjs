#![allow(dead_code)]

use ast::{Ast, Expr, ListHead, NodeId, PrimeExpr, Stmt};
use common::{span::Span, string::String};
use lexer::Lexer;
use token::{t, Token, TokenKind};

mod macros;

mod binding;
mod class;
mod error;
mod expr;
mod function;
mod prime;
mod stmt;

pub use error::{Error, ErrorKind};
/// Result type used troughout the parser
pub type Result<T> = std::result::Result<T, Error>;

/// The toyjs javascript parser.
pub struct Parser<'a> {
    /// The lexer used during parsing
    pub lexer: Lexer<'a>,
    /// The ast, in which the source parsed.
    pub ast: Ast,
    peek: Option<Token>,
    last_span: Span,
    ate_line_terminator: bool,
    state: ParserState,
}

/// Parser state.
///
/// Javascript syntax has a bunch of parameterized productions. This struct tracks the state of
/// those parameters.
#[derive(Debug)]
pub struct ParserState {
    pub strict: bool,
    pub yield_ident: bool,
    pub await_ident: bool,
    pub r#in: bool,
    pub r#break: bool,
    pub r#continue: bool,
}

impl<'a> Parser<'a> {
    /// Create a new parser.
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer,
            ast: Ast::default(),
            peek: None,
            last_span: Span::empty(),
            ate_line_terminator: false,
            state: ParserState {
                strict: false,
                yield_ident: true,
                await_ident: true,
                r#in: true,
                r#break: false,
                r#continue: false,
            },
        }
    }

    // Return the span
    fn last_span(&self) -> &Span {
        if let Some(x) = self.peek.as_ref().map(|x| &x.span) {
            x
        } else {
            &self.last_span
        }
    }

    /// Return the next token from the lexer which is not whitespace.
    #[inline(always)]
    fn retrieve_next_token(&mut self) -> Option<Token> {
        self.ate_line_terminator = false;
        while let Some(x) = self.lexer.next_token() {
            match x.kind_and_data.kind() {
                t!(" ") | t!("//") => {}
                t!("\n") => {
                    self.ate_line_terminator = true;
                }
                _ => return Some(x),
            }
        }
        None
    }

    /// Return the next token
    #[inline]
    fn next(&mut self) -> Option<Token> {
        let res = self.peek.take().or_else(|| self.retrieve_next_token());
        if let Some(x) = res.as_ref() {
            self.last_span = x.span.clone();
        }
        res
    }

    /// Returns the next token in the list without advancing the token iterator..
    #[inline]
    fn peek(&mut self) -> Option<Token> {
        if let Some(x) = self.peek.as_ref() {
            return Some(x.clone());
        }
        self.peek = self.retrieve_next_token();
        self.peek.clone()
    }

    /// Returns the next token in the list without advancing the token iterator..
    #[inline]
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
    #[inline]
    fn eat(&mut self, which: TokenKind) -> bool {
        if self.peek().map(|x| x.kind_and_data.kind()) == Some(which) {
            self.peek = None;
            true
        } else {
            false
        }
    }

    /// Mark a spot where a semicolon should be inserted.
    #[inline]
    fn eat_semicolon(&mut self) -> bool {
        match self.peek_kind() {
            Some(t!(";")) => {
                self.peek = None;
                true
            }
            Some(t!("}")) | None => true,
            Some(_) => self.ate_line_terminator,
        }
    }

    /// Mark a spot where a semicolon should be inserted.
    #[inline]
    fn semicolon(&mut self) -> Result<()> {
        if !self.eat_semicolon() {
            unexpected!(self, self.peek.clone().unwrap().kind(), ";");
        }
        Ok(())
    }

    /// Mark a spot where a semicolon should be inserted.
    #[inline]
    fn no_line_terminator(&mut self) -> Result<()> {
        if self.ate_line_terminator {
            unexpected!(self, self.peek.clone().unwrap().kind() => "new line before this token is not allowed");
        }
        Ok(())
    }

    /// Checks if a statement contains the strict directive `"use strict"`.
    #[inline]
    pub fn is_strict_directive(&self, stmt: NodeId<Stmt>) -> bool {
        let Stmt::Expr { expr } = self.ast[stmt] else {
            return false;
        };
        let Expr::Prime { expr } = self.ast[self.ast[expr].item] else {
            return false;
        };
        let PrimeExpr::String(id) = self.ast[expr] else {
            return false;
        };
        self.lexer.data.strings[id] == String::new_const("use strict")
    }

    /// Parse a javascript script.
    pub fn parse_script(&mut self) -> Result<ListHead<Stmt>> {
        let mut head = ListHead::Empty;
        let mut prev = None;
        while self.peek().is_some() {
            let stmt = self.parse_stmt()?;
            if !self.state.strict && head.is_empty() {
                self.state.strict = self.is_strict_directive(stmt);
            }
            prev = Some(self.ast.append_list(stmt, prev));
            head = head.or(prev.into())
        }
        Ok(head)
    }
}
