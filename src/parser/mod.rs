use crate::{
    ast::*,
    lexer::Lexer,
    token::{DelimToken, Kw, Span, Token, TokenKind},
};
use std::collections::VecDeque;

mod error;
use error::{ParseError, ParseErrorKind};
mod stmt;

type PResult<'a, T> = Result<T, ParseError<'a>>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    pending_tokens: VecDeque<Token<'a>>,
    prev_span: Span,
    strict_mode: bool,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer,
            prev_span: Span { lo: 0, hi: 0 },
            pending_tokens: VecDeque::new(),
            strict_mode: false,
        }
    }

    pub fn strict(&mut self, enabled: bool) -> &mut Self {
        self.strict_mode = enabled;
        self
    }

    pub fn cur_span(&self) -> Span {
        self.pending_tokens
            .front()
            .map(|e| e.span)
            .unwrap_or(self.prev_span)
    }

    pub fn todo(&self) -> PResult<'a, ()> {
        Err(ParseError {
            kind: ParseErrorKind::Todo,
            span: self.cur_span(),
        })
    }

    pub fn parse_script(&mut self) -> PResult<'a, Script<'a>> {
        let mut script = Script { stmts: Vec::new() };
        while self.peek().is_some() {
            script.stmts.push(self.parse_stmt()?);
            dbg!(&script);
            self.eat(TokenKind::SemiColon);
        }
        Ok(script)
    }

    pub fn parse_expr(&mut self) -> PResult<'a, Expr<'a>> {
        self.todo()?;
        todo!()
    }

    pub fn parse_assigment_expr(&mut self) -> PResult<'a, Expr<'a>> {
        self.parse_primary_expr()
    }

    pub fn parse_primary_expr(&mut self) -> PResult<'a, Expr<'a>> {
        if self.is_lit() {
            println!("LITERAL");
            return Ok(Expr::Literal {
                token: self.next().unwrap(),
            });
        }
        if self.is(tok!("true")) || self.is(tok!("false")) {
            return Ok(Expr::Literal {
                token: self.next().unwrap(),
            });
        }
        self.todo()?;
        todo!()
    }

    pub fn next(&mut self) -> Option<Token<'a>> {
        let res = if self.pending_tokens.is_empty() {
            return self.lexer.next();
        } else {
            self.pending_tokens.pop_front()
        };
        res.map(|e| self.prev_span = e.span);
        res
    }

    pub fn peek(&mut self) -> Option<Token<'a>> {
        dbg!(if self.pending_tokens.is_empty() {
            let t = self.lexer.next();
            t.map(|t| {
                self.pending_tokens.push_back(t);
                t
            })
        } else {
            self.pending_tokens.front().cloned()
        })
    }

    pub fn is_lit(&mut self) -> bool {
        let t = self.peek();
        if t.is_none() {
            return false;
        }
        let t = t.unwrap();
        match t.kind {
            TokenKind::Number { big: _, kind: _ } => true,
            TokenKind::String => true,
            _ => false,
        }
    }

    pub fn eat(&mut self, kind: TokenKind) -> bool {
        if self.is(kind) {
            self.next();
            return true;
        }
        return false;
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

    pub fn is(&mut self, kind: TokenKind) -> bool {
        let token = self.peek();
        token.map(|x| x.kind == kind).unwrap_or(false)
    }
}
