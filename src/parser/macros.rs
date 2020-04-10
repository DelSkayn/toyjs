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

macro_rules! to_do {
    ($s:expr) => {
        syntax_error!(
            $s,
            crate::parser::ParseErrorKind::Todo {
                line: line!(),
                file: file!(),
            }
        )
    };
}

macro_rules! syntax_error {
    ($s:expr,$err:expr) => {{
        return Err(crate::parser::ParseError {
            kind: $err,
            origin: $s.cur_span(),
            source: $s.source(),
        })
        .into();
    }};
}
macro_rules! unexpected{
    ($s:expr$(,$t:tt)* $(,)*) => {
        syntax_error!($s,crate::parser::ParseErrorKind::UnexpectedToken{
            found: $s.peek(),
            expected: &[$($t,)*],
        });
    };
}

macro_rules! no_lt {
    ($s:expr) => {{
        if $s
            .peek_with_lt()
            .map(|e| e.kind == tok!("\n"))
            .unwrap_or(false)
        {
            syntax_error!($s, crate::parser::ParseErrorKind::UnexpectedLineTerminator);
        }
    }};
}

macro_rules! expect{
    ($s:expr$(,$t:tt)* $(,)*) => {
        let p = $s.peek();
        match p.map(|e| e.kind){
            $(Some(tok!($t)) => {
                $s.next();
            })*
            _ => {
                syntax_error!($s,crate::parser::ParseErrorKind::UnexpectedToken{
                    found: p,
                    expected: &[$($t,)*]
                });
            }
        }
    }
}

macro_rules! trace_log {
    ($s:tt) => {
        #[cfg(debug_assertions)]
        trace!(concat!("BEGIN: ", $s));
        #[cfg(debug_assertions)]
        struct __Dropper;
        #[cfg(debug_assertions)]
        impl Drop for __Dropper {
            fn drop(&mut self) {
                trace!(concat!("END: ", $s));
            }
        }
        #[cfg(debug_assertions)]
        let __dropper = __Dropper;
    };
}
