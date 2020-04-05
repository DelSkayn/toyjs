mod kw;
pub use kw::*;
mod display;

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
pub enum BinOpToken {
    /// -
    Minus,
    /// +
    Plus,
    /// *
    Mul,
    /// **
    Exponentiate,
    /// /
    Div,
    /// //
    IntegerDiv,
    /// %
    Remainder,
    /// <<
    LeftShift,
    /// >>
    RightShift,
    /// >>>
    UnsignedRightShift,
    /// &
    BitwiseAnd,
    /// ^
    BitwiseXor,
    /// |
    BitwiseOr,
}

pub enum LitToken {
    String,
    Number { big: bool, kind: NumberKind },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind<'a> {
    Ident(&'a str),
    String,
    Kw(Kw),
    Number {
        big: bool,
        kind: NumberKind,
    },
    /// ;
    SemiColon,
    /// (, [ or {,
    DelimOpen(DelimToken),
    /// ), ] or },
    DelimClose(DelimToken),
    BinOp(BinOpToken),
    BinOpAssign(BinOpToken),
    /// ++
    AddOne,
    /// --
    SubractOne,
    /// **
    Exponentiate,
    /// **=
    ExponentiateAssign,
    /// ~
    BitwiseNot,
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
    /// !
    Not,
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
    Unknown,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub span: Span,
}
