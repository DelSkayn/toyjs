/// A short hand for writing TokenKind's
#[macro_export]
macro_rules! t {
    ("strict_directive"$($rest:tt)*) => {
        $crate::TokenKind::Ident($crate::interner::consts::STRICT_DIRECTIVE)
    };
    ("target"$($rest:tt)*) => {
        $crate::TokenKind::Ident($crate::interner::consts::TARGET)
    };
    ("get"$($rest:tt)*) => {
        $crate::TokenKind::Ident($crate::interner::consts::GET)
    };
    ("set"$($rest:tt)*) => {
        $crate::TokenKind::Ident($crate::interner::consts::SET)
    };
    ("static"$($rest:tt)*) => {
        $crate::TokenKind::Ident($crate::interner::consts::STATIC)
    };
    ("of"$($rest:tt)*) => {
        $crate::TokenKind::Ident($crate::interner::consts::OF)
    };
    ("import"$($rest:tt)*) => {
        $crate::TokenKind::Ident($crate::interner::consts::IMPORT)
    };
    ("meta"$($rest:tt)*) => {
        $crate::TokenKind::Ident($crate::interner::consts::META)
    };
    ("string",$x:ident) => {
        $crate::TokenKind::Lit($crate::LitToken::String($x))
    };
    ("number",$x:ident) => {
        $crate::TokenKind::Lit($crate::LitToken::Number($x))
    };
    ("literal",$x:ident) => {
        $crate::TokenKind::Lit($x)
    };
    ("ident",$x:ident) => {
        $crate::TokenKind::Ident($x)
    };
    ("string") => {
        $crate::TokenKind::Lit($crate::LitToken::String(_))
    };
    ("number"$($rest:tt)*) => {
        $crate::TokenKind::Lit($crate::LitToken::Number(_))
    };
    ("literal"$($rest:tt)*) => {
        $crate::TokenKind::Lit(_)
    };
    ("ident"$($rest:tt)*) => {
        $crate::TokenKind::Ident(_)
    };
    ("\n"$($rest:tt)*) => {
        $crate::TokenKind::LineTerminator
    };
    (";"$($rest:tt)*) => {
        $crate::TokenKind::SemiColon
    };
    ("++"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::AddOne)
    };
    ("--"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::SubractOne)
    };
    ("~"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::BitwiseNot)
    };
    ("!"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::Not)
    };
    ("<"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::Less)
    };
    ("<="$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::LessEqual)
    };
    (">"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::Greater)
    };
    (">="$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::GreaterEqual)
    };
    ("="$($rest:tt)*) => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::Assign)
    };
    ("=>"$($rest:tt)*) => {
        $crate::TokenKind::Arrow
    };
    ("=="$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::Equal)
    };
    ("==="$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::StrictEqual)
    };
    ("!="$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::NotEqual)
    };
    ("!=="$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::StrictNotEqual)
    };
    ("."$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::Dot)
    };
    (".."$($rest:tt)*) => {
        $crate::TokenKind::DotDot
    };
    ("..."$($rest:tt)*) => {
        $crate::TokenKind::DotDotDot
    };
    (":"$($rest:tt)*) => {
        $crate::TokenKind::Colon
    };
    ("::"$($rest:tt)*) => {
        $crate::TokenKind::DoubleColon
    };
    ("&&"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::And)
    };
    ("||"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::Or)
    };
    ("?"$($rest:tt)*) => {
        $crate::TokenKind::Tenary
    };
    ("?."$($rest:tt)*) => {
        $crate::TokenKind::TenaryNull
    };
    ("??"$($rest:tt)*) => {
        $crate::TokenKind::NullCoalescing
    };
    (","$($rest:tt)*) => {
        $crate::TokenKind::Comma
    };
    ("("$($rest:tt)*) => {
        $crate::TokenKind::DelimOpen($crate::Delim::Paren)
    };
    ("["$($rest:tt)*) => {
        $crate::TokenKind::DelimOpen($crate::Delim::Bracket)
    };
    ("{"$($rest:tt)*) => {
        $crate::TokenKind::DelimOpen($crate::Delim::Brace)
    };
    (")"$($rest:tt)*) => {
        $crate::TokenKind::DelimClose($crate::Delim::Paren)
    };
    ("]"$($rest:tt)*) => {
        $crate::TokenKind::DelimClose($crate::Delim::Bracket)
    };
    ("}"$($rest:tt)*) => {
        $crate::TokenKind::DelimClose($crate::Delim::Brace)
    };
    ("-="$($rest:tt)*) => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::Minus)
    };
    ("+="$($rest:tt)*) => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::Plus)
    };
    ("*="$($rest:tt)*) => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::Mul)
    };
    ("**="$($rest:tt)*) => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::Exponentiate)
    };
    ("/="$($rest:tt)*) => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::Div)
    };
    ("//="$($rest:tt)*) => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::IntegerDiv)
    };
    ("%="$($rest:tt)*) => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::Remainder)
    };
    ("<<="$($rest:tt)*) => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::LeftShift)
    };
    (">>="$($rest:tt)*) => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::RightShift)
    };
    (">>>="$($rest:tt)*) => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::UnsignedRightShift)
    };
    ("&="$($rest:tt)*) => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::BitwiseAnd)
    };
    ("^="$($rest:tt)*) => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::BitwiseXor)
    };
    ("|="$($rest:tt)*) => {
        $crate::TokenKind::AssignOperator($crate::AssignOperator::BitwiseOr)
    };
    ("-"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::Minus)
    };
    ("+"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::Plus)
    };
    ("*"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::Mul)
    };
    ("**"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::Exponentiate)
    };
    ("/"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::Div)
    };
    ("//"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::IntegerDiv)
    };
    ("%"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::Remainder)
    };
    ("<<"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::LeftShift)
    };
    (">>"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::RightShift)
    };
    (">>>"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::UnsignedRightShift)
    };
    ("&"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::BitwiseAnd)
    };
    ("^"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::BitwiseXor)
    };
    ("|"$($rest:tt)*) => {
        $crate::TokenKind::Operator($crate::Operator::BitwiseOr)
    };
    ("await"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Await)
    };
    ("break"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Break)
    };
    ("case"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Case)
    };
    ("catch"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Catch)
    };
    ("class"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Class)
    };
    ("let"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Let)
    };
    ("const"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Const)
    };
    ("continue"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Continue)
    };
    ("debugger"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Debugger)
    };
    ("default"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Default)
    };
    ("delete"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Delete)
    };
    ("do"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Do)
    };
    ("else"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Else)
    };
    ("enum"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Enum)
    };
    ("export"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Export)
    };
    ("extends"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Extends)
    };
    ("false"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::False)
    };
    ("finally"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Finally)
    };
    ("for"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::For)
    };
    ("function"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Function)
    };
    ("if"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::If)
    };
    ("import"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Import)
    };
    ("in"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::In)
    };
    ("instanceof"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Instanceof)
    };
    ("new"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::New)
    };
    ("null"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Null)
    };
    ("return"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Return)
    };
    ("super"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Super)
    };
    ("switch"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Switch)
    };
    ("this"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::This)
    };
    ("throw"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Throw)
    };
    ("true"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::True)
    };
    ("try"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Try)
    };
    ("typeof"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Typeof)
    };
    ("var"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Var)
    };
    ("void"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Void)
    };
    ("while"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::While)
    };
    ("with"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::With)
    };
    ("yield"$($rest:tt)*) => {
        $crate::TokenKind::Keyword($crate::Keyword::Yield)
    };
}
