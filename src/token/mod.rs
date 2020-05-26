use crate::source::Span;

mod display;
mod kw;
mod op;

pub use kw::*;
pub use op::{BinOpToken, RelationToken, UnaryOpToken};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum NumberKind<'a> {
    Invalid(&'static str),
    Integer(i32),
    Float(f64),
    Big(&'a str),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DelimToken {
    /// ( or )
    Paren,
    /// [ or ]
    Bracket,
    /// { or }
    Brace,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LitToken<'a> {
    String(&'a str),
    Number(NumberKind<'a>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind<'a> {
    Ident(&'a str),
    Lit(LitToken<'a>),
    Kw(Kw),
    /// ;
    SemiColon,
    /// (, [ or {,
    DelimOpen(DelimToken),
    /// ), ] or },
    DelimClose(DelimToken),
    BinOp(BinOpToken),
    BinOpAssign(BinOpToken),
    UnaryOp(UnaryOpToken),
    Relation(RelationToken),
    /// =
    Assign,
    /// .
    Dot,
    /// ..
    DotDot,
    /// ...
    DotDotDot,
    /// :
    Colon,
    /// ::
    DoubleColon,
    /// ?
    Tenary,
    /// ?.
    TenaryNull,
    /// ??
    NullCoalescing,
    /// ,
    Comma,
    ///
    LineTerminator,
    Unknown,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub span: Span,
}
