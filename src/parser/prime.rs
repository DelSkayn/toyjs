use crate::{
    lexer::Lexer,
    parser::*,
    source::{Source, Span},
    ssa::{Null, SsaVar},
    token::{LitToken, NumberKind, Token, TokenKind},
};

impl<'a> Parser<'a> {
    pub fn parse_prime_expr(&mut self) -> PResult<SsaVar> {
        let peek = if let Some(x) = self.peek_kind()? {
            x
        } else {
            unexpected!(self => "primary expression expected found EOF");
        };
        match peek {
            TokenKind::Ident(x) => {
                self.next()?;
                let obj = self.builder.push_instruction(Instruction::LoadGlobal);
                let key = self.builder.load_constant(x);
                let res = self.builder.push_instruction(Instruction::ObjectGet {
                    key: key.into(),
                    object: obj.into(),
                });
                return Ok(res);
            }
            t!("true") => {
                self.next()?;
                Ok(self.builder.load_constant(true))
            }
            t!("false") => {
                self.next()?;
                Ok(self.builder.load_constant(false))
            }
            t!("null") => {
                self.next()?;
                Ok(self.builder.load_constant(Null))
            }
            t!("literal") => self.parse_literal(),
            t!("[") => self.parse_array_expr(),
            t!("{") => self.parse_object_expr(),
            t!("class") => self.parse_class(false),
            _ => to_do!(self),
        }
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
