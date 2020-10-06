use bumpalo::Bump;
use common::{interner::Interner, source::Source};
use toyjs_parser::Parser;

fn create_parser(string: &str, f: F) -> (Source, Interner, Bump)
where
    F: FnOnce(Parser),
{
    let source = Source::from_string(string.to_string());
    let interner = Interner::new();
    let bump = Bump::new();
    let parser = Parser::from_source(source, interner, bump);
    f(parser)
}

fn basic_expression() {
    create_parser("1 + 1", |p| {
        p.parse_script().unwrap();
    })
}
