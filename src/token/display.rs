use super::{BinOpToken, DelimToken, LitToken, TokenKind, UnaryOpToken};
use std::fmt;

impl fmt::Display for UnaryOpToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            UnaryOpToken::Negative => write!(f, "-"),
            UnaryOpToken::Positive => write!(f, "+"),
            UnaryOpToken::AddOne => write!(f, "++"),
            UnaryOpToken::SubractOne => write!(f, "--"),
            UnaryOpToken::Not => write!(f, "!"),
            UnaryOpToken::BitwiseNot => write!(f, "!"),
            UnaryOpToken::Void => write!(f, "void"),
            UnaryOpToken::Delete => write!(f, "delete"),
            UnaryOpToken::Typeof => write!(f, "typeof"),
        }
    }
}

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
        match *self {
            TokenKind::Ident(x) => write!(f, "{}", x),
            TokenKind::Lit(LitToken::String) => write!(f, "String"),
            TokenKind::Kw(x) => write!(f, "{}", x),
            TokenKind::Lit(LitToken::Number { big: _, kind: _ }) => write!(f, "number"),
            TokenKind::SemiColon => write!(f, ";"),
            TokenKind::DelimOpen(x) => match x {
                DelimToken::Brace => write!(f, "{{"),
                DelimToken::Bracket => write!(f, "["),
                DelimToken::Paren => write!(f, "("),
            },
            TokenKind::DelimClose(x) => match x {
                DelimToken::Brace => write!(f, "}}"),
                DelimToken::Bracket => write!(f, "]"),
                DelimToken::Paren => write!(f, ")"),
            },
            TokenKind::UnaryOp(x) => write!(f, "{}", x),
            TokenKind::Less => write!(f, "<"),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::Assign => write!(f, "="),
            TokenKind::Arrow => write!(f, "=>"),
            TokenKind::Equal => write!(f, "=="),
            TokenKind::StrictEqual => write!(f, "==="),
            TokenKind::NotEqual => write!(f, "!="),
            TokenKind::StrictNotEqual => write!(f, "!=="),
            TokenKind::Dot => write!(f, "."),
            TokenKind::DotDot => write!(f, ".."),
            TokenKind::DotDotDot => write!(f, "..."),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::DoubleColon => write!(f, "::"),
            TokenKind::And => write!(f, "&&"),
            TokenKind::Or => write!(f, "||"),
            TokenKind::Tenary => write!(f, "?"),
            TokenKind::TenaryNull => write!(f, "?."),
            TokenKind::NullCoalescing => write!(f, "??"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Unknown => write!(f, "unknown"),
            TokenKind::BinOp(x) => write!(f, "{}", x),
            TokenKind::BinOpAssign(x) => write!(f, "{}=", x),
            TokenKind::LineTerminator => write!(f, "\\n"),
        }
    }
}
