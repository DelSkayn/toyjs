/// A short hand for writing token kinds
#[macro_export]
macro_rules! t {
    ("strict_directive") => {
        crate::token::TokenKind::Ident(crate::interner::consts::STRICT_DIRECTIVE)
    };
    ("target") => {
        crate::token::TokenKind::Ident(crate::interner::consts::TARGET)
    };
    ("get") => {
        crate::token::TokenKind::Ident(crate::interner::consts::GET)
    };
    ("set") => {
        crate::token::TokenKind::Ident(crate::interner::consts::SET)
    };
    ("static") => {
        crate::token::TokenKind::Ident(crate::interner::consts::STATIC)
    };
    ("of") => {
        crate::token::TokenKind::Ident(crate::interner::consts::OF)
    };
    ("import") => {
        crate::token::TokenKind::Ident(crate::interner::consts::IMPORT)
    };
    ("meta") => {
        crate::token::TokenKind::Ident(crate::interner::consts::META)
    };
    ("string") => {
        crate::token::TokenKind::Lit(crate::token::LitToken::String(_))
    };
    ("number") => {
        crate::token::TokenKind::Lit(crate::token::LitToken::Number(_))
    };
    ("literal") => {
        crate::token::TokenKind::Lit(_)
    };
    ("ident") => {
        crate::token::TokenKind::Ident(_)
    };
    ("\n") => {
        crate::token::TokenKind::LineTerminator
    };
    (";") => {
        crate::token::TokenKind::SemiColon
    };
    ("++") => {
        crate::token::TokenKind::UnaryOp(crate::token::UnaryOpToken::AddOne)
    };
    ("--") => {
        crate::token::TokenKind::UnaryOp(crate::token::UnaryOpToken::SubractOne)
    };
    ("~") => {
        crate::token::TokenKind::UnaryOp(crate::token::UnaryOpToken::BitwiseNot)
    };
    ("!") => {
        crate::token::TokenKind::UnaryOp(crate::token::UnaryOpToken::Not)
    };
    ("<") => {
        crate::token::TokenKind::Relation(crate::token::RelationToken::Less)
    };
    ("<=") => {
        crate::token::TokenKind::Relation(crate::token::RelationToken::LessEqual)
    };
    (">") => {
        crate::token::TokenKind::Relation(crate::token::RelationToken::Greater)
    };
    (">=") => {
        crate::token::TokenKind::Relation(crate::token::RelationToken::GreaterEqual)
    };
    ("=") => {
        crate::token::TokenKind::Assign
    };
    ("=>") => {
        crate::token::TokenKind::Arrow
    };
    ("==") => {
        crate::token::TokenKind::Relation(crate::token::RelationToken::Equal)
    };
    ("===") => {
        crate::token::TokenKind::Relation(crate::token::RelationToken::StrictEqual)
    };
    ("!=") => {
        crate::token::TokenKind::Relation(crate::token::RelationToken::NotEqual)
    };
    ("!==") => {
        crate::token::TokenKind::Relation(crate::token::RelationToken::StrictNotEqual)
    };
    (".") => {
        crate::token::TokenKind::Dot
    };
    ("..") => {
        crate::token::TokenKind::DotDot
    };
    ("...") => {
        crate::token::TokenKind::DotDotDot
    };
    (":") => {
        crate::token::TokenKind::Colon
    };
    ("::") => {
        crate::token::TokenKind::DoubleColon
    };
    ("&&") => {
        crate::token::TokenKind::Relation(crate::token::RelationToken::And)
    };
    ("||") => {
        crate::token::TokenKind::Relation(crate::token::RelationToken::Or)
    };
    ("?") => {
        crate::token::TokenKind::Tenary
    };
    ("?.") => {
        crate::token::TokenKind::TenaryNull
    };
    ("??") => {
        crate::token::TokenKind::NullCoalescing
    };
    (",") => {
        crate::token::TokenKind::Comma
    };
    ("(") => {
        crate::token::TokenKind::DelimOpen(crate::token::DelimToken::Paren)
    };
    ("[") => {
        crate::token::TokenKind::DelimOpen(crate::token::DelimToken::Bracket)
    };
    ("{") => {
        crate::token::TokenKind::DelimOpen(crate::token::DelimToken::Brace)
    };
    (")") => {
        crate::token::TokenKind::DelimClose(crate::token::DelimToken::Paren)
    };
    ("]") => {
        crate::token::TokenKind::DelimClose(crate::token::DelimToken::Bracket)
    };
    ("}") => {
        crate::token::TokenKind::DelimClose(crate::token::DelimToken::Brace)
    };
    ("-=") => {
        crate::token::TokenKind::BinOpAssign(crate::token::BinOpToken::Minus)
    };
    ("+=") => {
        crate::token::TokenKind::BinOpAssign(crate::token::BinOpToken::Plus)
    };
    ("*=") => {
        crate::token::TokenKind::BinOpAssign(crate::token::BinOpToken::Mul)
    };
    ("**=") => {
        crate::token::TokenKind::BinOpAssign(crate::token::BinOpToken::Exponentiate)
    };
    ("/=") => {
        crate::token::TokenKind::BinOpAssign(crate::token::BinOpToken::Div)
    };
    ("//=") => {
        crate::token::TokenKind::BinOpAssign(crate::token::BinOpToken::IntegerDiv)
    };
    ("%=") => {
        crate::token::TokenKind::BinOpAssign(crate::token::BinOpToken::Remainder)
    };
    ("<<=") => {
        crate::token::TokenKind::BinOpAssign(crate::token::BinOpToken::LeftShift)
    };
    (">>=") => {
        crate::token::TokenKind::BinOpAssign(crate::token::BinOpToken::RightShift)
    };
    (">>>=") => {
        crate::token::TokenKind::BinOpAssign(crate::token::BinOpToken::UnsignedRightShift)
    };
    ("&=") => {
        crate::token::TokenKind::BinOpAssign(crate::token::BinOpToken::BitwiseAnd)
    };
    ("^=") => {
        crate::token::TokenKind::BinOpAssign(crate::token::BinOpToken::BitwiseXor)
    };
    ("|=") => {
        crate::token::TokenKind::BinOpAssign(crate::token::BinOpToken::BitwiseOr)
    };
    ("-") => {
        crate::token::TokenKind::BinOp(crate::token::BinOpToken::Minus)
    };
    ("+") => {
        crate::token::TokenKind::BinOp(crate::token::BinOpToken::Plus)
    };
    ("u-") => {
        crate::token::TokenKind::UnaryOp(crate::token::UnaryOpToken::Negative)
    };
    ("u+") => {
        crate::token::TokenKind::UnaryOp(crate::token::UnaryOpToken::Positive)
    };
    ("*") => {
        crate::token::TokenKind::BinOp(crate::token::BinOpToken::Mul)
    };
    ("**") => {
        crate::token::TokenKind::BinOp(crate::token::BinOpToken::Exponentiate)
    };
    ("/") => {
        crate::token::TokenKind::BinOp(crate::token::BinOpToken::Div)
    };
    ("//") => {
        crate::token::TokenKind::BinOp(crate::token::BinOpToken::IntegerDiv)
    };
    ("%") => {
        crate::token::TokenKind::BinOp(crate::token::BinOpToken::Remainder)
    };
    ("<<") => {
        crate::token::TokenKind::BinOp(crate::token::BinOpToken::LeftShift)
    };
    (">>") => {
        crate::token::TokenKind::BinOp(crate::token::BinOpToken::RightShift)
    };
    (">>>") => {
        crate::token::TokenKind::BinOp(crate::token::BinOpToken::UnsignedRightShift)
    };
    ("&") => {
        crate::token::TokenKind::BinOp(crate::token::BinOpToken::BitwiseAnd)
    };
    ("^") => {
        crate::token::TokenKind::BinOp(crate::token::BinOpToken::BitwiseXor)
    };
    ("|") => {
        crate::token::TokenKind::BinOp(crate::token::BinOpToken::BitwiseOr)
    };
    ("await") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Await)
    };
    ("break") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Break)
    };
    ("case") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Case)
    };
    ("catch") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Catch)
    };
    ("class") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Class)
    };
    ("let") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Let)
    };
    ("const") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Const)
    };
    ("continue") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Continue)
    };
    ("debugger") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Debugger)
    };
    ("default") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Default)
    };
    ("delete") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Delete)
    };
    ("do") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Do)
    };
    ("else") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Else)
    };
    ("enum") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Enum)
    };
    ("export") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Export)
    };
    ("extends") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Extends)
    };
    ("false") => {
        crate::token::TokenKind::Kw(crate::token::Kw::False)
    };
    ("finally") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Finally)
    };
    ("for") => {
        crate::token::TokenKind::Kw(crate::token::Kw::For)
    };
    ("function") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Function)
    };
    ("if") => {
        crate::token::TokenKind::Kw(crate::token::Kw::If)
    };
    ("import") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Import)
    };
    ("in") => {
        crate::token::TokenKind::Kw(crate::token::Kw::In)
    };
    ("instanceof") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Instanceof)
    };
    ("new") => {
        crate::token::TokenKind::Kw(crate::token::Kw::New)
    };
    ("null") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Null)
    };
    ("return") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Return)
    };
    ("super") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Super)
    };
    ("switch") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Switch)
    };
    ("this") => {
        crate::token::TokenKind::Kw(crate::token::Kw::This)
    };
    ("throw") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Throw)
    };
    ("true") => {
        crate::token::TokenKind::Kw(crate::token::Kw::True)
    };
    ("try") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Try)
    };
    ("typeof") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Typeof)
    };
    ("var") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Var)
    };
    ("void") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Void)
    };
    ("while") => {
        crate::token::TokenKind::Kw(crate::token::Kw::While)
    };
    ("with") => {
        crate::token::TokenKind::Kw(crate::token::Kw::With)
    };
    ("yield") => {
        crate::token::TokenKind::Kw(crate::token::Kw::Yield)
    };
}

macro_rules! shrinkwrap_index {
    ($name:ident) => {
        #[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
        pub struct $name(pub(crate) crate::util::Index);

        impl $name {
            pub const fn invalid() -> Self {
                Self(crate::util::Index::invalid())
            }

            pub fn into_usize(self) -> usize {
                self.0.into_usize()
            }
        }

        impl From<u8> for $name {
            fn from(v: u8) -> Self {
                Self(v.into())
            }
        }

        impl From<u16> for $name {
            fn from(v: u16) -> Self {
                Self(v.into())
            }
        }

        impl From<u32> for $name {
            fn from(v: u32) -> Self {
                Self(v.into())
            }
        }

        impl From<usize> for $name {
            fn from(v: usize) -> Self {
                Self(v.into())
            }
        }
    };
}
