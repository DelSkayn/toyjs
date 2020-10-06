/// A short hand for writing token kinds
#[macro_export]
macro_rules! t {
    ("strict_directive"$($rest:tt)*) => {
        $crate::token::TokenKind::Ident($crate::interner::consts::STRICT_DIRECTIVE)
    };
    ("target"$($rest:tt)*) => {
        $crate::token::TokenKind::Ident($crate::interner::consts::TARGET)
    };
    ("get"$($rest:tt)*) => {
        $crate::token::TokenKind::Ident($crate::interner::consts::GET)
    };
    ("set"$($rest:tt)*) => {
        $crate::token::TokenKind::Ident($crate::interner::consts::SET)
    };
    ("static"$($rest:tt)*) => {
        $crate::token::TokenKind::Ident($crate::interner::consts::STATIC)
    };
    ("of"$($rest:tt)*) => {
        $crate::token::TokenKind::Ident($crate::interner::consts::OF)
    };
    ("import"$($rest:tt)*) => {
        $crate::token::TokenKind::Ident($crate::interner::consts::IMPORT)
    };
    ("meta"$($rest:tt)*) => {
        $crate::token::TokenKind::Ident($crate::interner::consts::META)
    };
    ("string",$x:ident) => {
        $crate::token::TokenKind::Lit($crate::token::LitToken::String($x))
    };
    ("number",$x:ident) => {
        $crate::token::TokenKind::Lit($crate::token::LitToken::Number($x))
    };
    ("literal",$x:ident) => {
        $crate::token::TokenKind::Lit($x)
    };
    ("ident",$x:ident) => {
        $crate::token::TokenKind::Ident($x)
    };
    ("string") => {
        $crate::token::TokenKind::Lit($crate::token::LitToken::String(_))
    };
    ("number"$($rest:tt)*) => {
        $crate::token::TokenKind::Lit($crate::token::LitToken::Number(_))
    };
    ("literal"$($rest:tt)*) => {
        $crate::token::TokenKind::Lit(_)
    };
    ("ident"$($rest:tt)*) => {
        $crate::token::TokenKind::Ident(_)
    };
    ("\n"$($rest:tt)*) => {
        $crate::token::TokenKind::LineTerminator
    };
    (";"$($rest:tt)*) => {
        $crate::token::TokenKind::SemiColon
    };
    ("++"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::AddOne)
    };
    ("--"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::SubractOne)
    };
    ("~"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::BitwiseNot)
    };
    ("!"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::Not)
    };
    ("<"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::Less)
    };
    ("<="$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::LessEqual)
    };
    (">"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::Greater)
    };
    (">="$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::GreaterEqual)
    };
    ("="$($rest:tt)*) => {
        $crate::token::TokenKind::AssignOperator($crate::token::AssignOperator::Assign)
    };
    ("=>"$($rest:tt)*) => {
        $crate::token::TokenKind::Arrow
    };
    ("=="$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::Equal)
    };
    ("==="$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::StrictEqual)
    };
    ("!="$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::NotEqual)
    };
    ("!=="$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::StrictNotEqual)
    };
    ("."$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::Dot)
    };
    (".."$($rest:tt)*) => {
        $crate::token::TokenKind::DotDot
    };
    ("..."$($rest:tt)*) => {
        $crate::token::TokenKind::DotDotDot
    };
    (":"$($rest:tt)*) => {
        $crate::token::TokenKind::Colon
    };
    ("::"$($rest:tt)*) => {
        $crate::token::TokenKind::DoubleColon
    };
    ("&&"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::And)
    };
    ("||"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::Or)
    };
    ("?"$($rest:tt)*) => {
        $crate::token::TokenKind::Tenary
    };
    ("?."$($rest:tt)*) => {
        $crate::token::TokenKind::TenaryNull
    };
    ("??"$($rest:tt)*) => {
        $crate::token::TokenKind::NullCoalescing
    };
    (","$($rest:tt)*) => {
        $crate::token::TokenKind::Comma
    };
    ("("$($rest:tt)*) => {
        $crate::token::TokenKind::DelimOpen($crate::token::Delim::Paren)
    };
    ("["$($rest:tt)*) => {
        $crate::token::TokenKind::DelimOpen($crate::token::Delim::Bracket)
    };
    ("{"$($rest:tt)*) => {
        $crate::token::TokenKind::DelimOpen($crate::token::Delim::Brace)
    };
    (")"$($rest:tt)*) => {
        $crate::token::TokenKind::DelimClose($crate::token::Delim::Paren)
    };
    ("]"$($rest:tt)*) => {
        $crate::token::TokenKind::DelimClose($crate::token::Delim::Bracket)
    };
    ("}"$($rest:tt)*) => {
        $crate::token::TokenKind::DelimClose($crate::token::Delim::Brace)
    };
    ("-="$($rest:tt)*) => {
        $crate::token::TokenKind::AssignOperator($crate::token::AssignOperator::Minus)
    };
    ("+="$($rest:tt)*) => {
        $crate::token::TokenKind::AssignOperator($crate::token::AssignOperator::Plus)
    };
    ("*="$($rest:tt)*) => {
        $crate::token::TokenKind::AssignOperator($crate::token::AssignOperator::Mul)
    };
    ("**="$($rest:tt)*) => {
        $crate::token::TokenKind::AssignOperator($crate::token::AssignOperator::Exponentiate)
    };
    ("/="$($rest:tt)*) => {
        $crate::token::TokenKind::AssignOperator($crate::token::AssignOperator::Div)
    };
    ("//="$($rest:tt)*) => {
        $crate::token::TokenKind::AssignOperator($crate::token::AssignOperator::IntegerDiv)
    };
    ("%="$($rest:tt)*) => {
        $crate::token::TokenKind::AssignOperator($crate::token::AssignOperator::Remainder)
    };
    ("<<="$($rest:tt)*) => {
        $crate::token::TokenKind::AssignOperator($crate::token::AssignOperator::LeftShift)
    };
    (">>="$($rest:tt)*) => {
        $crate::token::TokenKind::AssignOperator($crate::token::AssignOperator::RightShift)
    };
    (">>>="$($rest:tt)*) => {
        $crate::token::TokenKind::AssignOperator($crate::token::AssignOperator::UnsignedRightShift)
    };
    ("&="$($rest:tt)*) => {
        $crate::token::TokenKind::AssignOperator($crate::token::AssignOperator::BitwiseAnd)
    };
    ("^="$($rest:tt)*) => {
        $crate::token::TokenKind::AssignOperator($crate::token::AssignOperator::BitwiseXor)
    };
    ("|="$($rest:tt)*) => {
        $crate::token::TokenKind::AssignOperator($crate::token::AssignOperator::BitwiseOr)
    };
    ("-"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::Minus)
    };
    ("+"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::Plus)
    };
    ("*"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::Mul)
    };
    ("**"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::Exponentiate)
    };
    ("/"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::Div)
    };
    ("//"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::IntegerDiv)
    };
    ("%"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::Remainder)
    };
    ("<<"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::LeftShift)
    };
    (">>"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::RightShift)
    };
    (">>>"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::UnsignedRightShift)
    };
    ("&"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::BitwiseAnd)
    };
    ("^"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::BitwiseXor)
    };
    ("|"$($rest:tt)*) => {
        $crate::token::TokenKind::Operator($crate::token::Operator::BitwiseOr)
    };
    ("await"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Await)
    };
    ("break"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Break)
    };
    ("case"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Case)
    };
    ("catch"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Catch)
    };
    ("class"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Class)
    };
    ("let"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Let)
    };
    ("const"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Const)
    };
    ("continue"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Continue)
    };
    ("debugger"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Debugger)
    };
    ("default"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Default)
    };
    ("delete"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Delete)
    };
    ("do"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Do)
    };
    ("else"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Else)
    };
    ("enum"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Enum)
    };
    ("export"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Export)
    };
    ("extends"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Extends)
    };
    ("false"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::False)
    };
    ("finally"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Finally)
    };
    ("for"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::For)
    };
    ("function"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Function)
    };
    ("if"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::If)
    };
    ("import"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Import)
    };
    ("in"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::In)
    };
    ("instanceof"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Instanceof)
    };
    ("new"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::New)
    };
    ("null"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Null)
    };
    ("return"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Return)
    };
    ("super"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Super)
    };
    ("switch"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Switch)
    };
    ("this"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::This)
    };
    ("throw"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Throw)
    };
    ("true"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::True)
    };
    ("try"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Try)
    };
    ("typeof"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Typeof)
    };
    ("var"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Var)
    };
    ("void"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Void)
    };
    ("while"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::While)
    };
    ("with"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::With)
    };
    ("yield"$($rest:tt)*) => {
        $crate::token::TokenKind::Keyword($crate::token::Keyword::Yield)
    };
}
