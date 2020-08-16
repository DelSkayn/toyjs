use crate::{
    interner::StringId,
    lexer::Lexer,
    parser::*,
    source::{Source, Span},
    ssa::{Constant, Expr, Null, SsaBuilder, SsaId},
    token::{LitToken, NumberKind, Token, TokenKind},
};

impl<'a> Parser<'a> {
    pub fn parse_prime_expr(&mut self, builder: &mut SsaBuilder) -> PResult<Expr> {
        let peek = if let Some(x) = self.peek_kind()? {
            x
        } else {
            unexpected!(self => "primary expression expected found EOF");
        };
        let value = match peek {
            TokenKind::Ident(x) => {
                self.next()?;
                builder.reference(x)
            }
            t!("true") => {
                self.next()?;
                builder.constant(Constant::Boolean(true))
            }
            t!("false") => {
                self.next()?;
                builder.constant(Constant::Boolean(false))
            }
            t!("null") => {
                self.next()?;
                builder.constant(Constant::Null)
            }
            t!("literal") => self.parse_literal(builder)?,
            t!("[") => self.parse_array_expr(builder)?,
            t!("{") => self.parse_object_expr(builder)?,
            t!("(") => {
                self.next()?;
                let res = self.parse_expr(builder)?;
                expect!(self, ")");
                return Ok(res.into());
            }
            t!("class") => self.parse_class(false, builder)?,
            _ => to_do!(self),
        };
        Ok(value)
    }

    fn parse_literal(&mut self, builder: &mut SsaBuilder) -> PResult<Expr> {
        let lit = if let TokenKind::Lit(x) = self.next()?.unwrap().kind {
            x
        } else {
            panic!("parse function called wrong!");
        };
        match lit {
            LitToken::String(x) => Ok(builder.constant(Constant::String(x))),
            LitToken::Number(x) => match x {
                NumberKind::Integer(x) => Ok(builder.constant(Constant::Integer(x))),
                NumberKind::Float(x) => Ok(builder.constant(Constant::Float(x))),
                NumberKind::Big(_) => to_do!(self),
            },
        }
    }

    fn parse_array_expr(&mut self, _builder: &mut SsaBuilder) -> PResult<Expr> {
        to_do!(self)
    }
    fn parse_object_expr(&mut self, _builder: &mut SsaBuilder) -> PResult<Expr> {
        to_do!(self)
    }
    fn parse_class(&mut self, _ehhhh: bool, _builder: &mut SsaBuilder) -> PResult<Expr> {
        to_do!(self)
    }
}
