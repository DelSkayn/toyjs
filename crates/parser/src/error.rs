use common::span::Span;
use token::TokenKind;

#[derive(Clone, Debug)]
pub enum ErrorKind {
    /// The parser unexpectedly encounted the end of source.
    UnexpectedEnd {
        expected: Vec<TokenKind>,
        message: Option<String>,
    },
    Unexpected {
        expected: Vec<TokenKind>,
        found: TokenKind,
        message: Option<String>,
    },
    InvalidToken,
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub origin: Span,
}
