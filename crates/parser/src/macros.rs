#[macro_export]
macro_rules! apply_msg {
    ($msg:expr) => {
        Some(String::from($msg))
    };
    () => {
        None
    };
}

/// render the part of the ast and return it.
#[macro_export]
macro_rules! dbg_tree {
    ($parser:expr,$e:expr) => {{
        let expr = $e;
        eprintln!(
            "DBG: {}",
            ast::RenderAst::display(
                &expr,
                ast::RenderCtx::new(
                    &$parser.ast,
                    &$parser.lexer.data.strings,
                    &$parser.lexer.data.numbers,
                )
            )
        );
        expr
    }};
}

/// Alter the state of the render for the duration of scope.
///
/// # Usage
/// ```
/// alter_state!(self, r#break = true, r#continue = true => {
///     // break and continue are allowed in this statement.
///     let stmt = self.parse_stmt()
///     // Dont break out of this scope by for example returning.
///     // It will not restore the previous state of the parser.
/// });
/// // previous state is restored here.
/// ```
#[macro_export]
macro_rules! alter_state{
    ($parser:expr $(,$name:ident = $e:expr)* => { $($t:tt)* }) => {
        $(
            let $name = $parser.state.$name;
            $parser.state.$name = $e;
        )*
            $($t)*
        $($parser.state.$name = $name;)*
    }
}

/// Peeks the next token raising an error if there is no next token.
///
/// # Usage
/// ```
/// peek_expect!(self,"ident","other expected tokens")
/// ```
#[macro_export]
macro_rules! peek_expect {
    ($parser:expr$(,$($expect:tt),*$(,)?)? $(=> $msg:expr)?) => {
        {
            let Some(token) = $parser.peek() else{
                return Err($crate::error::Error::new(
                    $crate::error::ErrorKind::UnexpectedEnd{
                        expected: vec![$($($crate::t!($expect),)*)*],
                        message: peek_expect!(@msg $($msg)?),
                    },
                    $parser.last_span().clone(),
                ));
            };
            token
        }
    };
    (@msg $msg:expr) => {
        Some(common::string::String::from($msg))
    };
    (@msg) => {
        None
    };
}

/// Returns the next token raising an error if there is no next token.
///
/// # Usage
/// ```
/// next_expect!(self,"ident","other expected tokens")
/// ```
#[macro_export]
macro_rules! next_expect {
    ($parser:expr$(,$expect:tt)* $(=> $msg:expr)?) => {
        {
            let Some(token) = $parser.next() else {
                $crate::unexpected_end!($parser$(,$expect)* $(=> $msg)?);
            };
            token
        }
    };
    (@msg $msg:expr) => {
        Some(String::from($msg))
    };
    (@msg) => {
        None
    };
}

/// Raising an unexpected error
///
/// # Usage
/// ```
/// unexpected!(self,"token which was found","ident","other tokens which where expected" => "error message for more context");
/// // Message is optional
/// unexpected!(self,"token which was found","ident","other tokens which where expected");
/// ```
#[macro_export]
macro_rules! unexpected {
    ($parser:expr,$found:expr$( , $( $expect:tt),+ )? $(=> $msg:expr)?) => {
        if let token::TokenKind::Unknown = $found{
            return Err($crate::error::Error::new(
                $crate::error::ErrorKind::InvalidToken,
                $parser.last_span().clone(),
            ))
        }else{
            let expected = vec![$($( $crate::t!($expect),)*)*];
            return Err($crate::error::Error::new(
                 $crate::error::ErrorKind::Unexpected{
                    found: $found,
                    expected,
                    message: $crate::unexpected!(@msg $($msg)*)
                },
                 $parser.last_span().clone(),
            ))
        }
    };
    (@msg $msg:expr) => {
        Some(common::string::String::from($msg))
    };
    (@msg) => {
        None
    };
}

/// Raising an unexpected end error, notifying that source ended to early
///
/// # Usage
/// ```
/// unexpected_end!(self,"ident","other tokens which where expected" => "error message for more context");
/// // Message is optional
/// unexpected_end!(self,"ident","other tokens which where expected");
/// ```
#[macro_export]
macro_rules! unexpected_end {
    ($parser:expr$( , $( $expect:tt),+ )? $(=> $msg:expr)?) => {
        return Err($crate::error::Error::new(
             $crate::error::ErrorKind::UnexpectedEnd{
                expected: vec![$($($crate::t!($expect),)*)*],
                message: $crate::unexpected_end!(@msg $($msg)*),
            },
             $parser.last_span().clone(),
        ))
    };
    (@msg $msg:expr) => {
        Some(common::string::String::from($msg))
    };
    (@msg) => {
        None
    };
}

/// Expect the next token to be of a certain kind, raising an unexpected error if the token found
/// is different, returning it otherwise
///
/// # Usage
/// ```
/// let ident = expect!(self,"ident");
/// expect!(self,".");
/// let member = expect!(self,"ident");
/// ```

#[macro_export]
macro_rules! expect {
    ($parser:expr,$expect:tt$(=> $msg:expr)?) => {
        match $parser.peek_kind() {
            Some($crate::t!($expect)) => {
                $parser.next().unwrap()
            }
            Some(x) => {
                $crate::unexpected!($parser,x,$expect $(=> $msg)*)
            }
            None => {
                $crate::unexpected_end!($parser,$expect $(=> $msg)*)
            }
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
