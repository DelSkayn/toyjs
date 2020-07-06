macro_rules! to_do {
    ($span:expr) => {
        return Err(crate::compiler::CompilerError {
            span: $span,
            kind: crate::compiler::CompilerErrorKind::Todo,
        });
    };
}
