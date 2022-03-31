#![allow(dead_code)]
#![feature(allocator_api)]

use ast::symbol_table::{SymbolTable, SymbolTableBuilder};
use common::{
    interner::{Interner, StringId},
    source::Span,
};
use lexer::Lexer;
use token::{t, Token, TokenKind};

use std::{alloc::Allocator, result::Result as StdResult};

#[macro_use]
mod macros;

mod error;
pub use error::{Error, ErrorKind};

mod expr;
mod prime;
mod stmt;

pub type Result<T> = StdResult<T, Error>;

pub struct SpecialIdent {
    pub eval: StringId,
}

impl SpecialIdent {
    fn from_interner(int: &mut Interner) -> Self {
        SpecialIdent {
            eval: int.intern("eval"),
        }
    }
}

/// Parser state.
///
/// Certain keywords like `break` and `return` are only allowed inside blocks in
/// a certain context. This keeps struct of that context.
#[derive(Default, Clone)]
pub struct State {
    r#return: bool,
    r#break: bool,
    r#continue: bool,
}

/// The toyjs parser. Takes a set of tokens and produces an AST
pub struct Parser<'source, A: Allocator> {
    lexer: Lexer<'source>,
    peek: Option<Token>,
    peek_lt: Option<Token>,
    last_span: Span,
    symbol_table: SymbolTableBuilder<'source, A>,
    state: State,
    alloc: A,
    special_ident: SpecialIdent,
}

impl<'source, A: Allocator + Clone> Parser<'source, A> {
    pub fn parse_script(
        lexer: Lexer<'source>,
        symbol_table: &'source mut SymbolTable<A>,
        alloc: A,
    ) -> Result<ast::Script<A>> {
        Parser {
            special_ident: SpecialIdent::from_interner(lexer.interner),
            lexer,
            peek: None,
            peek_lt: None,
            last_span: Span { low: 0, hi: 0 },
            symbol_table: SymbolTableBuilder::new_script(symbol_table),
            alloc,
            state: State {
                r#return: false,
                r#break: false,
                r#continue: false,
            },
        }
        .do_parse_script()
    }

    pub fn parse_module(
        lexer: Lexer<'source>,
        symbol_table: &'source mut SymbolTable<A>,
        alloc: A,
    ) -> Result<ast::Script<A>> {
        Parser {
            special_ident: SpecialIdent::from_interner(lexer.interner),
            lexer,
            peek: None,
            peek_lt: None,
            last_span: Span { low: 0, hi: 0 },
            symbol_table: SymbolTableBuilder::new_module(symbol_table),
            alloc,
            state: State {
                r#return: false,
                r#break: false,
                r#continue: false,
            },
        }
        .do_parse_module()
    }

    fn alter_state<Fa, Fc, R>(&mut self, fa: Fa, fc: Fc) -> R
    where
        Fa: FnOnce(&mut State),
        Fc: FnOnce(&mut Self) -> R,
    {
        let clone = self.state.clone();
        fa(&mut self.state);
        let res = fc(self);
        self.state = clone;
        res
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
        if let Some(x) = self.peek_lt.take().or_else(|| self.peek.take()) {
            Ok(Some(x))
        } else {
            Ok(self.lexer.next()?.map(|e| {
                self.last_span = e.span;
                e
            }))
        }
    }

    fn peek_lt(&mut self) -> Result<Option<Token>> {
        if let Some(x) = self.peek_lt.or(self.peek) {
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
                Some(x) => {
                    if x.kind == t!("\n") {
                        self.peek_lt = Some(x);
                    } else {
                        self.last_span = x.span;
                        self.peek = Some(x);
                        return Ok(Some(x));
                    }
                }
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

    fn do_parse_script(mut self) -> Result<ast::Script<A>> {
        let mut stmts = Vec::new_in(self.alloc.clone());
        while self.peek()?.is_some() {
            stmts.push(self.parse_stmt()?);
        }
        Ok(ast::Script(stmts))
    }

    fn do_parse_module(self) -> Result<ast::Script<A>> {
        todo!()
    }
}
