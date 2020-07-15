use crate::{
    ast::*,
    lexer::Lexer,
    source::{Source, Span},
    token::{DelimToken, Token, TokenKind},
};

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self) -> Result<()> {
        self.parse_expr()
    }
}
