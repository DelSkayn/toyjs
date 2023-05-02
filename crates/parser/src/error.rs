use common::span::Span;
use token::TokenKind;

#[derive(Clone, Debug)]
pub enum ErrorKind {
    /// The parser unexpectedly encounted the end of source.
    UnexpectedEnd {
        expected: Vec<TokenKind>,
    },
    Unexpected {
        expected: Vec<TokenKind>,
        found: TokenKind,
    },
    InvalidToken,
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub origin: Span,
}
