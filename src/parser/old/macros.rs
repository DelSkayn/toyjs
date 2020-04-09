macro_rules! is {
    ($p:expr,$t:tt) => {
        match $p.peek().map(|e| e.kind) {
            Some(tok!($t)) => true,
            _ => false,
        }
    };
}

macro_rules! eat {
    ($p:expr,$t:tt) => {
        $p.eat(tok!($t))
    };
}

macro_rules! is_any {
    ($p:expr$(, $t:tt)+ $(,)*) => {
        if let Some(x) = $p.next(){
            match x.kind {
                $(tok!($t) => true,)*
                    _ => false
            }
        }else{
            false
        }
    };
}

macro_rules! to_do {
    ($s:expr) => {
        return Err(crate::parser::ParseError {
            kind: crate::parser::ParseErrorKind::Todo,
            span: $s.cur_span(),
            src: $s.source(),
        })
        .into();
    };
}

macro_rules! unexpected_end {
    ($s:expr) => {
        return Err(crate::parser::ParseError {
            kind: crate::parser::ParseErrorKind::UnexpectedEnd,
            span: $s.cur_span(),
            src: $s.source(),
        })
        .into();
    };
}
macro_rules! expect {
    ($s:expr, $t:tt) => {{
        let peek = $s.peek();
        match peek.map(|e| e.kind) {
            Some(tok!($t)) => {}
            Some(_) => {
                return Err(crate::parser::ParseError {
                    kind: crate::parser::ParseErrorKind::UnexpectedToken {
                        found: peek.unwrap(),
                        expected: &[$t],
                    },
                    src: $s.source(),
                    span: $s.cur_span(),
                })
                .into();
            }
            _ => {
                return Err(crate::parser::ParseError {
                    kind: crate::parser::ParseErrorKind::UnexpectedEnd,
                    span: $s.cur_span(),
                    src: $s.source(),
                })
                .into();
            }
        }
    }};
}

macro_rules! unexpected{
    ($s:expr $(,$t:tt)* $(,)*) => {
        {
        let peek = $s.peek();
        let err = match peek {
            Some(p) => Err(
                crate::parser::ParseError{
                    kind: crate::parser::ParseErrorKind::UnexpectedToken{
                        found: p,
                        expected: &[$($t,)*]
                    },
                    src: $s.source(),
                    span: $s.cur_span(),
            }),
            _ => return Err(crate::parser::ParseError{
                kind: crate::parser::ParseErrorKind::UnexpectedEnd,
                span: $s.cur_span(),
                src: $s.source(),
            })
        };
        return err.into()
        }
    }
}

macro_rules! close_delim {
    ($s:expr,$span:expr, $t:tt) => {
        if !eat!($s, $t) {
            return Err(crate::parser::ParseError {
                kind: crate::parser::ParseErrorKind::MissingClosingDelim {
                    open: $span,
                    kind: _get_close_delim!($t),
                },
                span: $s.cur_span(),
                src: $s.source(),
            })
            .into();
        }
    };
}

macro_rules! _get_close_delim {
    ("}") => {
        crate::token::DelimToken::Brace
    };
    ("]") => {
        crate::token::DelimToken::Bracket
    };
    (")") => {
        crate::token::DelimToken::Paren
    };
    ($t:tt) => {
        compile_error!("invalid use of close delim")
    };
}
