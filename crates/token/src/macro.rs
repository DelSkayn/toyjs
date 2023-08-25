/// A short hand for writing `TokenKind`'s
#[macro_export]
macro_rules! t {
    ("ident") => {
        $crate::TokenKind::Ident
    };
    ("string") => {
        $crate::TokenKind::String
    };
    ("123") => {
        $crate::TokenKind::Number
    };
    ("123n") => {
        $crate::TokenKind::BigInt
    };
    ("\n") => {
        $crate::TokenKind::LineTerminator
    };
    (";") => {
        $crate::TokenKind::SemiColon
    };
    ("++") => {
        $crate::TokenKind::Operator($crate::Operator::AddOne)
    };
    ("--") => {
        $crate::TokenKind::Operator($crate::Operator::SubractOne)
    };
    ("~") => {
        $crate::TokenKind::Operator($crate::Operator::BitwiseNot)
    };
    ("!") => {
        $crate::TokenKind::Operator($crate::Operator::Not)
    };
    ("<") => {
        $crate::TokenKind::Operator($crate::Operator::Less)
    };
    ("<=") => {
        $crate::TokenKind::Operator($crate::Operator::LessEqual)
    };
    (">") => {
        $crate::TokenKind::Operator($crate::Operator::Greater)
    };
    (">=") => {
        $crate::TokenKind::Operator($crate::Operator::GreaterEqual)
    };
    ("=") => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::Assign)
    };
    ("=>") => {
        $crate::TokenKind::Arrow
    };
    ("==") => {
        $crate::TokenKind::Operator($crate::Operator::Equal)
    };
    ("===") => {
        $crate::TokenKind::Operator($crate::Operator::StrictEqual)
    };
    ("!=") => {
        $crate::TokenKind::Operator($crate::Operator::NotEqual)
    };
    ("!==") => {
        $crate::TokenKind::Operator($crate::Operator::StrictNotEqual)
    };
    (".") => {
        $crate::TokenKind::Operator($crate::Operator::Dot)
    };
    ("..") => {
        $crate::TokenKind::DotDot
    };
    ("...") => {
        $crate::TokenKind::DotDotDot
    };
    (":") => {
        $crate::TokenKind::Colon
    };
    ("::") => {
        $crate::TokenKind::DoubleColon
    };
    ("&&") => {
        $crate::TokenKind::Operator($crate::Operator::And)
    };
    ("||") => {
        $crate::TokenKind::Operator($crate::Operator::Or)
    };
    ("?") => {
        $crate::TokenKind::Tenary
    };
    ("?.") => {
        $crate::TokenKind::TenaryNull
    };
    ("??") => {
        $crate::TokenKind::NullCoalescing
    };
    (",") => {
        $crate::TokenKind::Comma
    };
    ("(") => {
        $crate::TokenKind::DelimOpen($crate::Delim::Paren)
    };
    ("[") => {
        $crate::TokenKind::DelimOpen($crate::Delim::Bracket)
    };
    ("{") => {
        $crate::TokenKind::DelimOpen($crate::Delim::Brace)
    };
    (")") => {
        $crate::TokenKind::DelimClose($crate::Delim::Paren)
    };
    ("]") => {
        $crate::TokenKind::DelimClose($crate::Delim::Bracket)
    };
    ("}") => {
        $crate::TokenKind::DelimClose($crate::Delim::Brace)
    };
    ("-=") => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::Minus)
    };
    ("+=") => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::Plus)
    };
    ("*=") => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::Mul)
    };
    ("**=") => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::Exponentiate)
    };
    ("/=") => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::Div)
    };
    ("%=") => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::Remainder)
    };
    ("<<=") => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::LeftShift)
    };
    (">>=") => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::RightShift)
    };
    (">>>=") => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::UnsignedRightShift)
    };
    ("&=") => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::BitwiseAnd)
    };
    ("^=") => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::BitwiseXor)
    };
    ("|=") => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::BitwiseOr)
    };
    ("-") => {
        $crate::TokenKind::Operator($crate::Operator::Minus)
    };
    ("+") => {
        $crate::TokenKind::Operator($crate::Operator::Plus)
    };
    ("*") => {
        $crate::TokenKind::Operator($crate::Operator::Mul)
    };
    ("**") => {
        $crate::TokenKind::Operator($crate::Operator::Exponentiate)
    };
    ("/") => {
        $crate::TokenKind::Operator($crate::Operator::Div)
    };
    ("%") => {
        $crate::TokenKind::Operator($crate::Operator::Remainder)
    };
    ("<<") => {
        $crate::TokenKind::Operator($crate::Operator::LeftShift)
    };
    (">>") => {
        $crate::TokenKind::Operator($crate::Operator::RightShift)
    };
    (">>>") => {
        $crate::TokenKind::Operator($crate::Operator::UnsignedRightShift)
    };
    ("&") => {
        $crate::TokenKind::Operator($crate::Operator::BitwiseAnd)
    };
    ("^") => {
        $crate::TokenKind::Operator($crate::Operator::BitwiseXor)
    };
    ("|") => {
        $crate::TokenKind::Operator($crate::Operator::BitwiseOr)
    };
    ("break") => {
        $crate::TokenKind::Keyword($crate::Keyword::Break)
    };
    ("case") => {
        $crate::TokenKind::Keyword($crate::Keyword::Case)
    };
    ("catch") => {
        $crate::TokenKind::Keyword($crate::Keyword::Catch)
    };
    ("class") => {
        $crate::TokenKind::Keyword($crate::Keyword::Class)
    };
    ("const") => {
        $crate::TokenKind::Keyword($crate::Keyword::Const)
    };
    ("continue") => {
        $crate::TokenKind::Keyword($crate::Keyword::Continue)
    };
    ("debugger") => {
        $crate::TokenKind::Keyword($crate::Keyword::Debugger)
    };
    ("default") => {
        $crate::TokenKind::Keyword($crate::Keyword::Default)
    };
    ("delete") => {
        $crate::TokenKind::Keyword($crate::Keyword::Delete)
    };
    ("do") => {
        $crate::TokenKind::Keyword($crate::Keyword::Do)
    };
    ("else") => {
        $crate::TokenKind::Keyword($crate::Keyword::Else)
    };
    ("enum") => {
        $crate::TokenKind::Keyword($crate::Keyword::Enum)
    };
    ("export") => {
        $crate::TokenKind::Keyword($crate::Keyword::Export)
    };
    ("extends") => {
        $crate::TokenKind::Keyword($crate::Keyword::Extends)
    };
    ("false") => {
        $crate::TokenKind::Keyword($crate::Keyword::False)
    };
    ("finally") => {
        $crate::TokenKind::Keyword($crate::Keyword::Finally)
    };
    ("for") => {
        $crate::TokenKind::Keyword($crate::Keyword::For)
    };
    ("function") => {
        $crate::TokenKind::Keyword($crate::Keyword::Function)
    };
    ("if") => {
        $crate::TokenKind::Keyword($crate::Keyword::If)
    };
    ("import") => {
        $crate::TokenKind::Keyword($crate::Keyword::Import)
    };
    ("in") => {
        $crate::TokenKind::Keyword($crate::Keyword::In)
    };
    ("instanceof") => {
        $crate::TokenKind::Keyword($crate::Keyword::Instanceof)
    };
    ("new") => {
        $crate::TokenKind::Keyword($crate::Keyword::New)
    };
    ("null") => {
        $crate::TokenKind::Keyword($crate::Keyword::Null)
    };
    ("return") => {
        $crate::TokenKind::Keyword($crate::Keyword::Return)
    };
    ("super") => {
        $crate::TokenKind::Keyword($crate::Keyword::Super)
    };
    ("switch") => {
        $crate::TokenKind::Keyword($crate::Keyword::Switch)
    };
    ("this") => {
        $crate::TokenKind::Keyword($crate::Keyword::This)
    };
    ("throw") => {
        $crate::TokenKind::Keyword($crate::Keyword::Throw)
    };
    ("true") => {
        $crate::TokenKind::Keyword($crate::Keyword::True)
    };
    ("try") => {
        $crate::TokenKind::Keyword($crate::Keyword::Try)
    };
    ("typeof") => {
        $crate::TokenKind::Keyword($crate::Keyword::Typeof)
    };
    ("var") => {
        $crate::TokenKind::Keyword($crate::Keyword::Var)
    };
    ("void") => {
        $crate::TokenKind::Keyword($crate::Keyword::Void)
    };
    ("while") => {
        $crate::TokenKind::Keyword($crate::Keyword::While)
    };
    ("with") => {
        $crate::TokenKind::Keyword($crate::Keyword::With)
    };
    ("await") => {
        $crate::TokenKind::UnreservedKeyword($crate::UnreservedKeyword::Await)
    };
    ("yield") => {
        $crate::TokenKind::UnreservedKeyword($crate::UnreservedKeyword::Yield)
    };
    ("let") => {
        $crate::TokenKind::UnreservedKeyword($crate::UnreservedKeyword::Let)
    };
    ("static") => {
        $crate::TokenKind::UnreservedKeyword($crate::UnreservedKeyword::Static)
    };
    ("implements") => {
        $crate::TokenKind::UnreservedKeyword($crate::UnreservedKeyword::Implements)
    };
    ("interface") => {
        $crate::TokenKind::UnreservedKeyword($crate::UnreservedKeyword::Interface)
    };
    ("package") => {
        $crate::TokenKind::UnreservedKeyword($crate::UnreservedKeyword::Package)
    };
    ("private") => {
        $crate::TokenKind::UnreservedKeyword($crate::UnreservedKeyword::Private)
    };
    ("protected") => {
        $crate::TokenKind::UnreservedKeyword($crate::UnreservedKeyword::Protected)
    };
    ("public") => {
        $crate::TokenKind::UnreservedKeyword($crate::UnreservedKeyword::Public)
    };
    // Always allowed as identifiers
    ("as") => {
        $crate::TokenKind::UnreservedKeyword($crate::UnreservedKeyword::As)
    };
    ("async") => {
        $crate::TokenKind::UnreservedKeyword($crate::UnreservedKeyword::Async)
    };
    ("from") => {
        $crate::TokenKind::UnreservedKeyword($crate::UnreservedKeyword::From)
    };
    ("get") => {
        $crate::TokenKind::UnreservedKeyword($crate::UnreservedKeyword::Get)
    };
    ("meta") => {
        $crate::TokenKind::UnreservedKeyword($crate::UnreservedKeyword::Meta)
    };
    ("of") => {
        $crate::TokenKind::UnreservedKeyword($crate::UnreservedKeyword::Of)
    };
    ("set") => {
        $crate::TokenKind::UnreservedKeyword($crate::UnreservedKeyword::Set)
    };
    ("target") => {
        $crate::TokenKind::UnreservedKeyword($crate::UnreservedKeyword::Target)
    };
    ("regex") => {
        $crate::TokenKind::Regex
    };
    ("//") => {
        $crate::TokenKind::Comment
    };
    (" ") => {
        $crate::TokenKind::Whitespace
    };
    ("``") => {
        $crate::TokenKind::Template($crate::Template::NoSubstitute)
    };
    ("` ${") => {
        $crate::TokenKind::Template($crate::Template::Start)
    };
    ("} ${") => {
        $crate::TokenKind::Template($crate::Template::Middle)
    };
    ("} `") => {
        $crate::TokenKind::Template($crate::Template::End)
    };
}
