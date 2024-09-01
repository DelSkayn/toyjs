#![allow(dead_code)]

use ast::{Ast, Expr, ListHead, NodeId, NodeListId, PrimeExpr, Stmt};
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
    peek: Option<Token>,
    last_span: Span,
    ate_line_terminator: bool,
    state: ParserState,
}

bitflags::bitflags! {
    /// Parser state.
    ///
    /// Javascript syntax has a bunch of parameterized productions. This struct tracks the state of
    /// those parameters.
    #[repr(transparent)]
    #[derive(Debug,Clone,Copy,PartialEq,Eq,Hash)]
    pub struct ParserState: u8 {
        const Strict     = 0b100000;
        const YieldIdent = 0b010000;
        const AwaitIdent = 0b001000;
        const In         = 0b000100;
        const Break      = 0b000010;
        const Continue   = 0b000001;
    }
}

pub trait Parse {
    fn parse(parser: &mut Parser, ast: &mut Ast) -> Result<NodeId<Self>>;
}

pub struct ParsedScript {
    pub strict: bool,
    pub stmt: Option<NodeListId<Stmt>>,
}

impl<'a> Parser<'a> {
    /// Create a new parser.
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer,
            peek: None,
            last_span: Span::empty(),
            ate_line_terminator: false,
            state: ParserState::In | ParserState::YieldIdent | ParserState::AwaitIdent,
        }
    }

    pub fn ast(&self) -> &Ast {
        &self.ast
    }

    pub fn ast_mut(&mut self) -> &mut Ast {
        &mut self.ast
    }

    pub fn into_ast(self) -> Ast {
        self.ast
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
    fn retrieve_next_token(&mut self) -> Token {
        self.ate_line_terminator = false;
        loop {
            let token = self.lexer.next_token();
            match token.kind_and_data.kind() {
                t!("\n") => {
                    self.ate_line_terminator = true;
                }
                _ => return token,
            }
        }
    }

    /// Return the next token
    fn next(&mut self) -> Token {
        let res = self
            .peek
            .take()
            .unwrap_or_else(|| self.retrieve_next_token());
        self.last_span = res.span;
        res
    }

    /// Returns the next token in the list without advancing the token iterator..
    fn peek(&mut self) -> Token {
        if let Some(peek) = self.peek.clone() {
            peek
        } else {
            let res = self.retrieve_next_token();
            self.peek = Some(res.clone());
            res
        }
    }

    /// Returns the next token in the list w
    /// ithout advancing the token iterator..
    fn peek_kind(&mut self) -> TokenKind {
        if let Some(peek) = self.peek.as_ref() {
            peek.kind()
        } else {
            let res = self.retrieve_next_token();
            self.peek = Some(res.clone());
            res.kind()
        }
    }

    /// Eat a token if it is of a specific type.
    /// Returns true if a token was eaten.
    fn eat(&mut self, which: TokenKind) -> bool {
        if self.peek().kind_and_data.kind() == which {
            self.peek = None;
            true
        } else {
            false
        }
    }

    /// Mark a spot where a semicolon should be inserted.
    fn eat_semicolon(&mut self) -> bool {
        match self.peek_kind() {
            t!(";") => {
                self.peek = None;
                true
            }
            t!("}") | t!("eof") => true,
            _ => self.ate_line_terminator,
        }
    }

    /// Mark a spot where a semicolon should be inserted.
    fn semicolon(&mut self) -> Result<()> {
        if !self.eat_semicolon() {
            unexpected!(self, self.peek.clone().unwrap().kind(), ";");
        }
        Ok(())
    }

    /// Mark a spot where a semicolon should be inserted.
    fn no_line_terminator(&mut self) -> Result<()> {
        if self.ate_line_terminator {
            unexpected!(self, self.peek.clone().expect("no_line_terminator should be called before consuming the next token").kind() => "a new line before this token is not allowed");
        }
        Ok(())
    }

    pub fn save_state<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let state = self.state;
        let r = f(self);
        self.state = state;
    }

    pub fn parse<P: Parse>(&mut self, ast: &mut Ast) -> Result<NodeId<P>> {
        P::parse(self, ast)
    }
}

impl Parse for ParsedScript {
    /// Parse a javascript script.
    fn parse(parser: &mut Parser, ast: &mut Ast) -> Result<ParsedScript> {
        let mut head = None;
        let mut cur = None;
        let mut strict = false;
        loop {
            let peek = parser.peek();
            if peek.kind() == t!("eof") {
                break;
            }
            let stmt = parser.parse(ast)?;
            if !parser.state.contains(ParserState::Strict)
                && head.is_none()
                && is_strict_directive(ast, stmt)
            {
                strict = true;
                parser.state.insert(ParserState::Strict);
            }
            ast.push_list(&mut head, &mut cur, stmt);
        }
        Ok(ParsedScript { stmt: head, strict })
    }
}

/// Checks if a statement contains the strict directive `"use strict"`.
fn is_strict_directive(ast: &Ast, stmt: NodeId<Stmt>) -> bool {
    let Stmt::Expr { expr } = ast[stmt] else {
        return false;
    };
    let Expr::Prime { expr } = expr.index(ast).item.index(ast) else {
        return false;
    };
    let PrimeExpr::String(id) = ast[expr] else {
        return false;
    };
    ast[lexer] == String::new_const("use strict")
}
