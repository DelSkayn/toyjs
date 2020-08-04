use crate::{
    lexer::Lexer,
    source::{Source, Span},
    ssa::{InstrVar, Instruction, SsaBuilder},
    token::{DelimToken, Token, TokenKind},
};

#[macro_use]
mod macros;

mod error;
pub use error::{ParseError, ParseErrorKind};

//mod class;
//mod decl;
mod decl;
mod expr;
mod ops;
mod prime;
mod stmt;

type PResult<T> = Result<T, ParseError>;

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
    peek: Option<Token>,
    pref_span: Span,
    state: StateFlags,
    pub builder: SsaBuilder,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer,
            peek: None,
            pref_span: Span { lo: 0, hi: 0 },
            state: StateFlags::default(),
            builder: SsaBuilder::new(),
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
            .map(|e| e.map(|e| e.kind == t!("\n")))
            .unwrap_or(None)
            .unwrap_or(false)
    }

    pub fn peek_with_lt(&mut self) -> PResult<Option<Token>> {
        let res = match self.peek.clone() {
            Some(x) => Some(x),
            None => {
                self.peek = self.lexer.next().map_err(|error| ParseError {
                    kind: ParseErrorKind::InvalidToken { error },
                    origin: self.cur_span(),
                })?;
                self.peek.clone()
            }
        };
        trace!("peek {:?}", res);
        Ok(res)
    }

    pub fn next_with_lt(&mut self) -> PResult<Option<Token>> {
        if let Some(x) = self.peek.take() {
            return Ok(Some(x));
        }
        let res = self.lexer.next().map_err(|error| ParseError {
            kind: ParseErrorKind::InvalidToken { error },
            origin: self.cur_span(),
        })?;
        res.as_ref().map(|e| self.pref_span = e.span);
        Ok(res)
    }

    pub fn eat_with_lt(&mut self, kind: TokenKind) -> PResult<bool> {
        if self.peek_with_lt()?.filter(|e| e.kind == kind).is_some() {
            self.next_with_lt().ok();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn peek(&mut self) -> PResult<Option<Token>> {
        while let Some(x) = self.peek_with_lt()? {
            if x.kind != t!("\n") {
                return Ok(Some(x));
            }
            self.next_with_lt()?;
        }
        Ok(None)
    }

    pub fn peek_kind(&mut self) -> PResult<Option<TokenKind>> {
        Ok(self.peek()?.map(|t| t.kind))
    }

    pub fn next(&mut self) -> PResult<Option<Token>> {
        while let Some(x) = self.next_with_lt()? {
            if x.kind != t!("\n") {
                return Ok(Some(x));
            }
        }
        Ok(None)
    }

    pub fn eat(&mut self, kind: TokenKind) -> PResult<bool> {
        if let Some(x) = self.peek()? {
            if x.kind == kind {
                self.next()?;
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub fn cur_span(&mut self) -> Span {
        if let Ok(Some(x)) = self.peek() {
            return x.span;
        }
        return self.pref_span;
    }

    /// Parse a js script.
    /// One of the 2 entry points into parsing
    pub fn parse_script(&mut self) -> PResult<()> {
        trace_log!("script");
        let mut last = None;
        while self.peek()?.is_some() {
            last = dbg!(self.parse_stmt()?);
            eat!(self, ";");
        }
        let value = last.map(|e| e.into()).unwrap_or(InstrVar::null());
        self.builder.push_instruction(Instruction::Return { value });
        Ok(())
    }

    /// Parse a js module.
    /// One of the 2 entry points into parsing
    pub fn parse_module(&mut self) -> PResult<()> {
        trace_log!("module");
        to_do!(self)
    }
}
