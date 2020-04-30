use std::fmt;

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum Kw {
    Await,
    Break,
    Case,
    Catch,
    Class,
    Let,
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

impl Kw {
    pub fn to_str(&self) -> &'static str {
        match self {
            Kw::Await => "await",
            Kw::Break => "break",
            Kw::Case => "case",
            Kw::Catch => "catch",
            Kw::Class => "class",
            Kw::Let => "let",
            Kw::Const => "const",
            Kw::Continue => "continue",
            Kw::Debugger => "debugger",
            Kw::Default => "default",
            Kw::Delete => "delete",
            Kw::Do => "do",
            Kw::Else => "else",
            Kw::Enum => "enum",
            Kw::Export => "export",
            Kw::Extends => "extends",
            Kw::False => "false",
            Kw::Finally => "finally",
            Kw::For => "for",
            Kw::Function => "function",
            Kw::If => "if",
            Kw::Import => "import",
            Kw::In => "in",
            Kw::Instanceof => "instanceof",
            Kw::New => "new",
            Kw::Null => "null",
            Kw::Return => "return",
            Kw::Super => "super",
            Kw::Switch => "switch",
            Kw::This => "this",
            Kw::Throw => "throw",
            Kw::True => "true",
            Kw::Try => "try",
            Kw::Typeof => "typeof",
            Kw::Var => "var",
            Kw::Void => "void",
            Kw::While => "while",
            Kw::With => "with",
        }
    }
}

impl fmt::Display for Kw {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}
