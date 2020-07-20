use crate::{
    lexer::Lexer,
    parser::*,
    source::{Source, Span},
    ssa::SsaVar,
    token::{DelimToken, Token, TokenKind},
};

impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> PResult<SsaVar> {
        let mut res = self.parse_ops()?;
        while eat!(self, ",") {
            res = self.parse_ops()?;
        }
        Ok(res)
    }
}
