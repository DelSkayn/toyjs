use crate::{
    interner::StringId,
    lexer::Lexer,
    parser::ops::Expr,
    parser::*,
    source::{Source, Span},
    ssa::{Null, SsaVar},
    token::{LitToken, NumberKind, Token, TokenKind},
};

pub enum PrimeExpr {
    Ident(StringId),
    Variable(SsaVar),
}

impl<'a> Parser<'a> {
    pub fn parse_prime_expr(&mut self) -> PResult<PrimeExpr> {
        let peek = if let Some(x) = self.peek_kind()? {
            x
        } else {
            unexpected!(self => "primary expression expected found EOF");
        };
        let value = match peek {
            TokenKind::Ident(x) => {
                self.next()?;
                return Ok(PrimeExpr::Ident(x));
            }
            t!("true") => {
                self.next()?;
                self.builder.load_constant(true)
            }
            t!("false") => {
                self.next()?;
                self.builder.load_constant(false)
            }
            t!("null") => {
                self.next()?;
                self.builder.load_constant(Null)
            }
            t!("literal") => self.parse_literal()?,
            t!("[") => self.parse_array_expr()?,
            t!("{") => self.parse_object_expr()?,
            t!("(") => {
                self.next()?;
                let res = self.parse_expr()?;
                expect!(self, ")");
                return Ok(PrimeExpr::Variable(res));
            }
            t!("class") => self.parse_class(false)?,
            _ => to_do!(self),
        };
        Ok(PrimeExpr::Variable(value))
    }

    fn parse_literal(&mut self) -> PResult<SsaVar> {
        let lit = if let TokenKind::Lit(x) = self.next()?.unwrap().kind {
            x
        } else {
            panic!("parse function called wrong!");
        };
        match lit {
            LitToken::String(x) => Ok(self.builder.load_constant(x)),
            LitToken::Number(x) => match x {
                NumberKind::Integer(x) => Ok(self.builder.load_constant(x)),
                NumberKind::Float(x) => Ok(self.builder.load_constant(x)),
                NumberKind::Big(_) => to_do!(self),
            },
        }
    }

    fn parse_array_expr(&mut self) -> PResult<SsaVar> {
        to_do!(self)
    }
    fn parse_object_expr(&mut self) -> PResult<SsaVar> {
        to_do!(self)
    }
    fn parse_class(&mut self, _ehhhh: bool) -> PResult<SsaVar> {
        to_do!(self)
    }
}
