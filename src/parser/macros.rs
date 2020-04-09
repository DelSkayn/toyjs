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
    ($s:expr,$err:expr) => {
        return Err(crate::parser::ParseError {
            kind: $err,
            origin: $s.cur_span(),
            source: $s.source(),
        })
        .into();
    };
}
