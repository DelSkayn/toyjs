use crate::{
    lexer::Lexer,
    parser::*,
    source::{Source, Span},
    ssa::{SsaBuilder, SsaId},
    token::{DelimToken, Token, TokenKind},
};

impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self, builder: &mut SsaBuilder) -> PResult<SsaId> {
        trace_log!("expression");
        let mut res = self.parse_ops(builder)?;
        while eat!(self, ",") {
            res = self.parse_ops(builder)?;
        }
        Ok(res)
    }
}
