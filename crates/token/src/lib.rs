use bytemuck::{AnyBitPattern, NoUninit};
use common::{id, span::Span};
use core::hash::{Hash, Hasher};

mod r#macro;

id!(pub struct StringId(u32));
id!(pub struct NumberId(u32));

/// A wrapper around f64 which implements bitwise equility and hashing.
#[derive(Clone, Copy, Debug)]
pub struct Number(pub f64);
impl From<&Number> for Number {
    fn from(value: &Number) -> Self {
        Number(value.0)
    }
}

impl Hash for Number {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state)
    }
}
impl Eq for Number {}
impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_bits() == other.0.to_bits()
    }
}

/// A token produced by the lexer.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Token {
    pub kind_and_data: TokenKindData,
    pub span: Span,
}

impl Token {
    #[inline]
    pub fn kind(&self) -> TokenKind {
        self.kind_and_data.kind()
    }

    #[inline]
    pub fn data_id<D: AnyBitPattern>(&self) -> Option<D> {
        self.kind_and_data.data_id()
    }
}

/// A packed data struct.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct TokenKindData(u64);

impl std::fmt::Debug for TokenKindData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TokenKindData")
            .field("kind", &self.kind())
            .field("data_id", &self.data_id::<u32>())
            .finish()
    }
}

/// The token kind and possible data id packed into 8 bytes
impl TokenKindData {
    #[inline]
    pub fn new<D: NoUninit>(kind: TokenKind, data_id: Option<D>) -> Self {
        Self::new_inner(kind, data_id.map(bytemuck::cast))
    }

    #[inline]
    fn new_inner(kind: TokenKind, data_id: Option<u32>) -> Self {
        let data_id = if let Some(x) = data_id {
            (x as u64 | (1 << 32)) << 16
        } else {
            0
        };

        let kind = unsafe { std::mem::transmute::<TokenKind, u16>(kind) };

        Self(data_id | (kind as u64))
    }

    #[inline]
    pub fn kind(self) -> TokenKind {
        unsafe { std::mem::transmute(self.0 as u16) }
    }

    #[inline]
    pub fn data_id<D: AnyBitPattern>(self) -> Option<D> {
        self.data_id_inner().map(bytemuck::cast)
    }

    #[inline]
    fn data_id_inner(self) -> Option<u32> {
        let data = self.0 >> 16;
        if data & (1 << 32) == 0 {
            None
        } else {
            Some(data as u32)
        }
    }
}

//Used to statically check the size of TokenKind.
#[allow(dead_code)]
const CHECK_TOKEN_KIND_SIZE: [u8; 2] = [0u8; std::mem::size_of::<TokenKind>()];

/// A possible javascript tokens.
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
#[repr(u8)]
pub enum TokenKind {
    Ident,
    String,
    Number,
    BigInt,
    Template(Template),
    Regex,
    UnreservedKeyword(UnreservedKeyword),
    Keyword(Keyword),
    /// `;`
    SemiColon,
    /// `(` or `[` or `{`
    DelimOpen(Delim),
    /// `)` or `]` or `}`
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
    /// `// comment ` or `/* comment */`
    Comment,
    Whitespace,
    Unknown,
}

/// A template token
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Template {
    /// `` ` bla till ${``
    Start,
    /// `} till ${`
    Middle,
    /// ``} till ` ``
    End,
    NoSubstitute,
}

// A delimitor.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Delim {
    /// `( or )`
    Paren,
    /// `\[` or `\]`
    Bracket,
    /// `{` or `}`
    Brace,
}

/// Reserved keywords which cannot be used as identifiers.
#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum Keyword {
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
    Instanceof,
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

/// Keywords which can be not reserved in certain contexts.
#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum UnreservedKeyword {
    // Disalowed contextually
    Await,
    Yield,
    // Disalowed in strict mode
    Let,
    Static,
    Implements,
    Interface,
    Package,
    Private,
    Protected,
    Public,
    // Always allowed as identifiers
    As,
    Async,
    From,
    Get,
    Meta,
    Of,
    Set,
    Target,
}

/// An javascript operator.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OperatorKind {
    Base(Operator),
    Assign(Operator),
}

/// An javascript operator.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

/// Javascript assignment operators
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AssignOperator {
    /// `=`
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
