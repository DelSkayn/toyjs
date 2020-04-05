use super::{BinOpToken, DelimToken, TokenKind};
use std::fmt;

impl fmt::Display for BinOpToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BinOpToken::Minus => write!(f, "-"),
            BinOpToken::Plus => write!(f, "+"),
            BinOpToken::Mul => write!(f, "*"),
            BinOpToken::Exponentiate => write!(f, "**"),
            BinOpToken::Div => write!(f, "/"),
            BinOpToken::IntegerDiv => write!(f, "//"),
            BinOpToken::Remainder => write!(f, "%"),
            BinOpToken::LeftShift => write!(f, "<<"),
            BinOpToken::RightShift => write!(f, ">>"),
            BinOpToken::UnsignedRightShift => write!(f, ">>>"),
            BinOpToken::BitwiseAnd => write!(f, "&"),
            BinOpToken::BitwiseXor => write!(f, "^"),
            BinOpToken::BitwiseOr => write!(f, "|"),
        }
    }
}

impl fmt::Display for TokenKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TokenKind::*;
        match *self {
            Ident(x) => write!(f, "{}", x),
            String => write!(f, "String"),
            Kw(x) => write!(f, "{}", x),
            Number { big: _, kind: _ } => write!(f, "number"),
            SemiColon => write!(f, ";"),
            DelimOpen(x) => match x {
                DelimToken::Brace => write!(f, "{{"),
                DelimToken::Bracket => write!(f, "["),
                DelimToken::Paren => write!(f, "("),
            },
            DelimClose(x) => match x {
                DelimToken::Brace => write!(f, "}}"),
                DelimToken::Bracket => write!(f, "]"),
                DelimToken::Paren => write!(f, ")"),
            },
            BinOp(x) => write!(f, "{}", x),
            BinOpAssign(x) => write!(f, "{}=", x),
            AddOne => write!(f, "++"),
            SubractOne => write!(f, "--"),
            Exponentiate => write!(f, "**"),
            ExponentiateAssign => write!(f, "**="),
            BitwiseNot => write!(f, "~"),
            Less => write!(f, "<"),
            LessEqual => write!(f, "<="),
            Greater => write!(f, ">"),
            GreaterEqual => write!(f, ">="),
            Assign => write!(f, "="),
            Arrow => write!(f, "=>"),
            Equal => write!(f, "=="),
            StrictEqual => write!(f, "==="),
            NotEqual => write!(f, "!="),
            StrictNotEqual => write!(f, "!=="),
            Not => write!(f, "!"),
            Dot => write!(f, "."),
            DotDot => write!(f, ".."),
            DotDotDot => write!(f, "..."),
            Colon => write!(f, ":"),
            DoubleColon => write!(f, "::"),
            And => write!(f, "&&"),
            Or => write!(f, "||"),
            Tenary => write!(f, "?"),
            TenaryNull => write!(f, "?."),
            NullCoalescing => write!(f, "??"),
            Comma => write!(f, ","),
            Unknown => write!(f, "unknown"),
        }
    }
}
