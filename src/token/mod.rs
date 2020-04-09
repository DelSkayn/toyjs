use crate::source::Span;

mod display;
mod kw;
mod op;

pub use kw::*;
pub use op::{BinOpToken, UnaryOpToken};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum NumberKind {
    Hex,
    Binary,
    Octal,
    Integer,
    Float,
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
pub enum LitToken {
    String,
    Number { big: bool, kind: NumberKind },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind<'a> {
    Ident(&'a str),
    Lit(LitToken),
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
    /// **
    Exponentiate,
    /// **=
    ExponentiateAssign,
    /// <
    Less,
    /// <=
    LessEqual,
    /// >
    Greater,
    /// >=
    GreaterEqual,
    /// =
    Assign,
    /// =>
    Arrow,
    /// ==
    Equal,
    /// ===
    StrictEqual,
    /// !=
    NotEqual,
    /// !==
    StrictNotEqual,
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
    /// &&
    And,
    /// ||
    Or,
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
