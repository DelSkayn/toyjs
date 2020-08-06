use crate::{interner::StringId, source::Span};

mod display;
mod kw;
mod op;

pub use kw::*;
pub use op::{BinOpToken, RelationToken, UnaryOpToken};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum NumberKind {
    Integer(i32),
    Float(f64),
    Big(StringId),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DelimToken {
    /// `( or )`
    Paren,
    /// `\[ or \]`
    Bracket,
    /// `{ or }`
    Brace,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LitToken {
    String(StringId),
    Number(NumberKind),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenKind {
    Ident(StringId),
    Lit(LitToken),
    Kw(Kw),
    /// `;`
    SemiColon,
    /// `(, [ or {,`
    DelimOpen(DelimToken),
    /// `), ] or },`
    DelimClose(DelimToken),
    BinOp(BinOpToken),
    BinOpAssign(BinOpToken),
    UnaryOp(UnaryOpToken),
    Relation(RelationToken),
    /// `=`
    Assign,
    /// `.`
    Dot,
    /// `..`
    DotDot,
    /// `...`
    DotDotDot,
    /// `:`
    Colon,
    /// `::`
    DoubleColon,
    /// `?`
    Tenary,
    /// `?.`
    TenaryNull,
    /// `??`
    NullCoalescing,
    /// `,`
    Comma,
    /// `\n`
    LineTerminator,
    Unknown,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}
