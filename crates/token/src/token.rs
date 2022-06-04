use common::{
    atom::{Atom, Atoms},
    source::Span,
};
use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Number {
    Integer(i32),
    Float(f64),
    Big(Atom),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Delim {
    /// `( or )`
    Paren,
    /// `\[ or \]`
    Bracket,
    /// `{ or }`
    Brace,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Literal {
    String(Atom),
    Number(Number),
}

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum Keyword {
    Await,
    Break,
    Case,
    Catch,
    Class,
    Const,
    Continue,
    Debugger,
    Default,
    Delete,
    Do,
    Else,
    Enum,
    Export,
    Extends,
    False,
    Finally,
    For,
    Function,
    If,
    Import,
    In,
    Of,
    Instanceof,
    Let,
    New,
    Null,
    Return,
    Super,
    Switch,
    This,
    Throw,
    True,
    Try,
    Typeof,
    Var,
    Void,
    While,
    With,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operator {
    /// `-`
    Minus,
    /// `+`
    Plus,
    /// `*`
    Mul,
    /// `**`
    Exponentiate,
    /// `/`
    Div,
    /// `%`
    Remainder,
    /// `<<`
    LeftShift,
    /// `>>`
    RightShift,
    /// `>>>`
    UnsignedRightShift,
    /// `&`
    BitwiseAnd,
    /// `^`
    BitwiseXor,
    /// `|`
    BitwiseOr,
    /// `++`
    AddOne,
    /// `--`
    SubractOne,
    /// `~`
    BitwiseNot,
    /// `!`
    Not,
    /// `delete`
    Delete,
    /// `void`
    Void,
    /// `typeof`
    Typeof,
    /// `<`
    Less,
    /// `<=`
    LessEqual,
    /// `>`
    Greater,
    /// `>=`
    GreaterEqual,
    /// `==`
    Equal,
    /// `===`
    StrictEqual,
    /// `!=`
    NotEqual,
    /// `!==`
    StrictNotEqual,
    /// `&&`
    And,
    /// `||`
    Or,
    /// `.`
    Dot,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AssignOperator {
    /// '='
    Assign,
    /// `-`
    Minus,
    /// `+`
    Plus,
    /// `*`
    Mul,
    /// `**`
    Exponentiate,
    /// `/`
    Div,
    /// `%`
    Remainder,
    /// `<<`
    LeftShift,
    /// `>>`
    RightShift,
    /// `>>>`
    UnsignedRightShift,
    /// `&`
    BitwiseAnd,
    /// `^`
    BitwiseXor,
    /// `|`
    BitwiseOr,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Template {
    TemplateHead(Atom),
    TemplateMiddle(Atom),
    TemplateTail(Atom),
    NoSubstitution(Atom),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenKind {
    Ident(Atom),
    Literal(Literal),
    Keyword(Keyword),
    /// `;`
    SemiColon,
    /// `(, [ or {,`
    DelimOpen(Delim),
    /// `), ] or },`
    DelimClose(Delim),
    Operator(Operator),
    AssignOperator(AssignOperator),
    /// `=>`
    Arrow,
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
    Template(Template),
}

impl TokenKind {
    pub fn format(self, atoms: &Atoms) -> FormatedTokenKind {
        FormatedTokenKind { kind: self, atoms }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Keyword {
    pub fn to_str(&self) -> &'static str {
        match self {
            Keyword::Await => "await",
            Keyword::Break => "break",
            Keyword::Case => "case",
            Keyword::Catch => "catch",
            Keyword::Class => "class",
            Keyword::Const => "const",
            Keyword::Continue => "continue",
            Keyword::Debugger => "debugger",
            Keyword::Default => "default",
            Keyword::Delete => "delete",
            Keyword::Do => "do",
            Keyword::Else => "else",
            Keyword::Enum => "enum",
            Keyword::Export => "export",
            Keyword::Extends => "extends",
            Keyword::False => "false",
            Keyword::Finally => "finally",
            Keyword::For => "for",
            Keyword::Function => "function",
            Keyword::If => "if",
            Keyword::Import => "import",
            Keyword::In => "in",
            Keyword::Of => "of",
            Keyword::Instanceof => "instanceof",
            Keyword::Let => "let",
            Keyword::New => "new",
            Keyword::Null => "null",
            Keyword::Return => "return",
            Keyword::Super => "super",
            Keyword::Switch => "switch",
            Keyword::This => "this",
            Keyword::Throw => "throw",
            Keyword::True => "true",
            Keyword::Try => "try",
            Keyword::Typeof => "typeof",
            Keyword::Var => "var",
            Keyword::Void => "void",
            Keyword::While => "while",
            Keyword::With => "with",
        }
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Operator::Minus => write!(f, "-"),
            Operator::Plus => write!(f, "+"),
            Operator::Mul => write!(f, "*"),
            Operator::Exponentiate => write!(f, "**"),
            Operator::Div => write!(f, "/"),
            Operator::Remainder => write!(f, "%"),
            Operator::LeftShift => write!(f, "<<"),
            Operator::RightShift => write!(f, ">>"),
            Operator::UnsignedRightShift => write!(f, ">>>"),
            Operator::BitwiseAnd => write!(f, "&"),
            Operator::BitwiseXor => write!(f, "^"),
            Operator::BitwiseOr => write!(f, "|"),
            Operator::AddOne => write!(f, "++"),
            Operator::SubractOne => write!(f, "--"),
            Operator::BitwiseNot => write!(f, "~"),
            Operator::Not => write!(f, "!"),
            Operator::Delete => write!(f, "delete"),
            Operator::Void => write!(f, "void"),
            Operator::Typeof => write!(f, "typeof"),
            Operator::Less => write!(f, "<"),
            Operator::LessEqual => write!(f, "<="),
            Operator::Greater => write!(f, ">"),
            Operator::GreaterEqual => write!(f, ">="),
            Operator::Equal => write!(f, "=="),
            Operator::StrictEqual => write!(f, "==="),
            Operator::NotEqual => write!(f, "!="),
            Operator::StrictNotEqual => write!(f, "!=="),
            Operator::And => write!(f, "&&"),
            Operator::Or => write!(f, "||"),
            Operator::Dot => write!(f, "."),
        }
    }
}

impl fmt::Display for AssignOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            AssignOperator::Minus => write!(f, "-="),
            AssignOperator::Plus => write!(f, "+="),
            AssignOperator::Mul => write!(f, "*="),
            AssignOperator::Exponentiate => write!(f, "**="),
            AssignOperator::Div => write!(f, "/="),
            AssignOperator::Remainder => write!(f, "%="),
            AssignOperator::LeftShift => write!(f, "<<="),
            AssignOperator::RightShift => write!(f, ">>="),
            AssignOperator::UnsignedRightShift => write!(f, ">>>="),
            AssignOperator::BitwiseAnd => write!(f, "&="),
            AssignOperator::BitwiseXor => write!(f, "^="),
            AssignOperator::BitwiseOr => write!(f, "|="),
            AssignOperator::Assign => write!(f, "="),
        }
    }
}

pub struct FormatedTokenKind<'a> {
    atoms: &'a Atoms,
    kind: TokenKind,
}

impl fmt::Display for FormatedTokenKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            TokenKind::Ident(x) => write!(
                f,
                "{}",
                &self
                    .atoms
                    .lookup(x)
                    .unwrap_or_else(|| "missing ident from interner!".to_string())
            ),
            TokenKind::Literal(x) => match x {
                Literal::String(_) => write!(f, "string"),
                Literal::Number(_) => write!(f, "number"),
            },
            TokenKind::Keyword(x) => write!(f, "{}", x),
            TokenKind::SemiColon => write!(f, ";"),
            TokenKind::DelimOpen(x) => match x {
                Delim::Paren => write!(f, "("),
                Delim::Bracket => write!(f, "["),
                Delim::Brace => write!(f, "{{"),
            },
            TokenKind::DelimClose(x) => match x {
                Delim::Paren => write!(f, ")"),
                Delim::Bracket => write!(f, "]"),
                Delim::Brace => write!(f, "}}"),
            },
            TokenKind::Operator(x) => write!(f, "{}", x),
            TokenKind::AssignOperator(x) => write!(f, "{}", x),
            TokenKind::Arrow => write!(f, "=>"),
            TokenKind::DotDot => write!(f, ".."),
            TokenKind::DotDotDot => write!(f, "..."),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::DoubleColon => write!(f, "::"),
            TokenKind::Tenary => write!(f, "?"),
            TokenKind::TenaryNull => write!(f, "?."),
            TokenKind::NullCoalescing => write!(f, "??"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::LineTerminator => write!(f, "\\n"),
            TokenKind::Template(Template::TemplateHead(_)) => write!(f, "` .. ${{"),
            TokenKind::Template(Template::TemplateMiddle(_)) => write!(f, "}} .. ${{"),
            TokenKind::Template(Template::TemplateTail(_)) => write!(f, "}} .. `"),
            TokenKind::Template(Template::NoSubstitution(_)) => write!(f, "` .. `"),
            TokenKind::Unknown => write!(f, "unknown"),
        }
    }
}
