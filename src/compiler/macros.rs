macro_rules! syntax_error {
    ($s:expr,$err:expr) => {{
        let e = crate::parser::CompilerError {
            kind: $err,
            origin: $s.cur_span(),
            source: $s.source(),
        };
        #[cfg(debug_assertions)]
        debug!("error in {}:{} :: {}\n", file!(), line!(), e);
        return Err(e).into();
    }};
}

macro_rules! to_do {
    ($s:expr) => {
        syntax_error!(
            $s,
            crate::compiler::CompilerErrorKind::Todo {
                line: line!(),
                file: file!(),
            }
        )
    };
}
