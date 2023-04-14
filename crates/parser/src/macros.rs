#[macro_export]
macro_rules! peek_expect {
    ($parser:expr$(,$($expect:tt),*$(,)?)?) => {
        {
            let Some(token) = $parser.peek() else{
                return Err($crate::error::Error{
                    kind: $crate::error::ErrorKind::UnexpectedEnd{ expected: vec![$($(t!($expect),)*)*] },
                    origin: $parser.last_span().clone(),
                });
            };
            token
        }
    };
}

#[macro_export]
macro_rules! next_expect {
    ($parser:expr$( , $( $expect:tt),+ )?) => {
        {
            let Some(token) = $parser.next() else{
                let expected = vec![$($( t!($expect),)*)*];
                return Err($crate::error::Error{
                    kind: $crate::error::ErrorKind::UnexpectedEnd{ expected },
                    origin: $parser.last_span().clone(),
                });
            };
            token
        }
    };
}

#[macro_export]
macro_rules! unexpected {
    ($parser:expr,$found:expr$( , $( $expect:tt),+ )?) => {
        if let token::TokenKind::Unknown = $found{
            return Err($crate::error::Error{
                kind: $crate::error::ErrorKind::InvalidToken,
                origin: $parser.last_span().clone(),
            })
        }else{
            let expected = vec![$($( t!($expect),)*)*];
            return Err($crate::error::Error{
                kind: $crate::error::ErrorKind::Unexpected{
                    found: $found,
                    expected
                },
                origin: $parser.last_span().clone(),
            })
        }
    };
}

#[macro_export]
macro_rules! expect {
    ($parser:expr,$expect:tt) => {
        if !$parser.eat(t!($expect)) {
            return Err($crate::error::Error {
                kind: $crate::error::ErrorKind::InvalidToken,
                origin: $parser.peek.as_ref().unwrap().span.clone(),
            });
        }
    };
}

#[macro_export]
macro_rules! match_next {
    ($parser:expr,match $token:ident {
        $($kind:tt => {
            $( $inner:tt)*
        })*
    }) => {
        let token = next_expect!($parser,$($kind),*);
        match token.kind(){
            $(
                t!($kind) => {
                    $($inner)*
                }
            )*
            x => {
                unexpected!($parser,x,$($kind),*)
            }
        }
    };
}

#[macro_export]
macro_rules! match_peek{
    ($parser:expr,match $token:ident {
        $($kind:tt => {
            $( $inner:tt)*
        })*
    }) => {
        let token = peek_expect!($parser,$($kind),*);
        match token.kind(){
            $(
                t!($kind) => {
                    $($inner)*
                }
            )*
            x => {
                unexpected!($parser,x,$($kind),*)
            }
        }
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! create_test_parser {
    ($source:expr,$parser:ident) => {
        let text = common::string::String::from_std_str($source);
        let lexer = lexer::Lexer::new(text.encoding());
        let mut $parser = $crate::Parser::new(lexer);
    };
}
