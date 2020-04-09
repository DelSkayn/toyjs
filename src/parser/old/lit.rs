use super::{PResult, Parser};
use crate::{
    ast::*,
    lexer::Lexer,
    token::{DelimToken, Kw, Span, Token, TokenKind, UnaryOpToken},
};

impl<'a> Parser<'a> {
    pub fn parse_array_lit(&mut self) -> PResult<'a, PrimeExpr<'a>> {
        to_do!(self)
    }

    pub fn parse_object_lit(&mut self) -> PResult<'a, PrimeExpr<'a>> {
        to_do!(self)
    }
}
