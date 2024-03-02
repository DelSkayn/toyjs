use std::{
    fmt::{self, Write},
    string::String as StdString,
};

use ast::{RenderAst, RenderCtx};
use common::{number::NumberId, result::ContextResultExt, string::String, structs::Interners};
use lexer::Lexer;
use parser::Parser;
use token::{t, TokenKind};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn lex(text: &str) -> Result<StdString, StdString> {
    Ok(lex_inner(text).unwrap())
}

fn lex_inner(text: &str) -> Result<StdString, fmt::Error> {
    let source = String::from_std_str(text);
    let mut interners = Interners::default();

    let mut lexer = Lexer::new(source.encoding(), &mut interners);
    let mut tokens = Vec::new();
    for t in lexer.by_ref() {
        tokens.push(t);
    }

    let mut res = StdString::new();

    for t in &tokens {
        let span = t.span;
        let kind = t.kind_and_data.kind();
        let kind_name = format!("{:?}", kind);
        write!(
            &mut res,
            "{:>4}+{:<4}:: {:<20}",
            span.offset(),
            span.size(),
            kind_name
        )?;
        write!(&mut res, "\t\t\t'{}'", source.encoding().slice(span))?;
        if kind == t!("ident") {
            let data = t.kind_and_data.data_id().unwrap();
            writeln!(
                &mut res,
                " = {:?} = '{}'",
                data,
                lexer.data.strings.get(data).unwrap()
            )?;
        } else if kind == t!("string") {
            let data = t.kind_and_data.data_id().unwrap();
            writeln!(
                &mut res,
                " = {:?}={:?}",
                data,
                lexer.data.strings.get(data).unwrap().as_str()
            )?;
        } else if kind == t!("123") {
            let data = t.kind_and_data.data_id::<NumberId>();
            writeln!(&mut res, " = {:e}", lexer.data.numbers[data.unwrap()].0)?;
        } else if kind == t!("123n") {
            let data = t.kind_and_data.data_id();
            writeln!(
                &mut res,
                " = {}n",
                lexer.data.strings.get(data.unwrap()).unwrap()
            )?;
        } else if let TokenKind::Template(_) = kind {
            let data = t.kind_and_data.data_id();
            writeln!(
                &mut res,
                " = `{}`",
                lexer.data.strings.get(data.unwrap()).unwrap()
            )?;
        } else {
            writeln!(&mut res,)?
        }
    }
    writeln!(
        &mut res,
        "> strings {}, numbers {}",
        lexer.data.strings.len(),
        lexer.data.numbers.len()
    )?;
    Ok(res)
}

#[wasm_bindgen]
pub fn parse(text: &str) -> Result<StdString, StdString> {
    let source = String::from_std_str(text);
    let source = common::source::Source::new(source, Some("parse_script"));
    let mut interners = Interners::default();
    let lexer = Lexer::new(source.source(), &mut interners);
    let mut parser = Parser::new(lexer);
    let res = parser.parse_script();
    match res.supply_context(&source) {
        Ok(x) => {
            let ctx = RenderCtx::new(
                &parser.ast,
                &parser.lexer.data.strings,
                &parser.lexer.data.numbers,
            );
            Ok(format!("{}", x.stmt.display(ctx)))
        }
        Err(e) => Err(format!("{}", e)),
    }
}
