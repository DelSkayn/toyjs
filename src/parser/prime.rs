use crate::{
    ast::*,
    lexer::Lexer,
    source::{Source, Span},
    token::{DelimToken, Token, TokenKind},
};

impl<'a> Parser<'a> {
    fn parse_prime_expr(&mut self) -> Result<()> {}
}
