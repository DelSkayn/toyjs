use bytemuck::{AnyBitPattern, NoUninit};
use common::{id, span::Span, string::String};
use core::hash::Hash;

mod r#macro;

id!(pub struct StringId(u32));

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
#[derive(Clone, Copy)]
pub struct TokenKindData(u64);

impl PartialEq for TokenKindData {
    fn eq(&self, other: &Self) -> bool {
        self.kind() == other.kind() && self.data_id_inner() == other.data_id_inner()
    }
}

impl Eq for TokenKindData {}

impl std::fmt::Debug for TokenKindData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TokenKindData")
            .field("kind", &self.kind())
            .field("data_id", &self.data_id::<u32>())
            .field("raw", &self.0)
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

impl Keyword {
    pub const fn to_string(self) -> String {
        match self {
            Self::Break => String::new_const("break"),
            Self::Case => String::new_const("case"),
            Self::Catch => String::new_const("catch"),
            Self::Class => String::new_const("class"),
            Self::Const => String::new_const("const"),
            Self::Continue => String::new_const("continue"),
            Self::Debugger => String::new_const("debugger"),
            Self::Default => String::new_const("default"),
            Self::Delete => String::new_const("delete"),
            Self::Do => String::new_const("do"),
            Self::Else => String::new_const("else"),
            Self::Enum => String::new_const("enum"),
            Self::Export => String::new_const("export"),
            Self::Extends => String::new_const("extends"),
            Self::False => String::new_const("false"),
            Self::Finally => String::new_const("finally"),
            Self::For => String::new_const("for"),
            Self::Function => String::new_const("function"),
            Self::If => String::new_const("if"),
            Self::Import => String::new_const("import"),
            Self::In => String::new_const("in"),
            Self::Instanceof => String::new_const("instanceof"),
            Self::New => String::new_const("new"),
            Self::Null => String::new_const("null"),
            Self::Return => String::new_const("return"),
            Self::Super => String::new_const("super"),
            Self::Switch => String::new_const("switch"),
            Self::This => String::new_const("this"),
            Self::Throw => String::new_const("throw"),
            Self::True => String::new_const("true"),
            Self::Try => String::new_const("try"),
            Self::Typeof => String::new_const("typeof"),
            Self::Var => String::new_const("var"),
            Self::Void => String::new_const("void"),
            Self::While => String::new_const("while"),
            Self::With => String::new_const("with"),
        }
    }
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

impl UnreservedKeyword {
    pub const fn to_string(self) -> String {
        match self {
            Self::Await => String::new_const("await"),
            Self::Yield => String::new_const("yield"),
            Self::Let => String::new_const("let"),
            Self::Static => String::new_const("static"),
            Self::Implements => String::new_const("implements"),
            Self::Interface => String::new_const("interface"),
            Self::Package => String::new_const("package"),
            Self::Private => String::new_const("private"),
            Self::Protected => String::new_const("protected"),
            Self::Public => String::new_const("public"),
            Self::As => String::new_const("as"),
            Self::Async => String::new_const("async"),
            Self::From => String::new_const("from"),
            Self::Get => String::new_const("get"),
            Self::Meta => String::new_const("meta"),
            Self::Of => String::new_const("of"),
            Self::Set => String::new_const("set"),
            Self::Target => String::new_const("target"),
        }
    }
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
