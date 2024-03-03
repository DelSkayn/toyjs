use std::string::String as StdString;

use ast::{RenderAst, RenderCtx};
use common::{result::ContextResultExt, string::String, structs::Interners};
use lexer::Lexer;
use parser::Parser;
use wasm_bindgen::prelude::*;

mod compile;
mod lex;

#[wasm_bindgen]
pub fn lex(text: &str) -> Result<StdString, StdString> {
    Ok(lex::lex(text).unwrap())
}

#[wasm_bindgen]
pub fn resolve(text: &str) -> Result<StdString, StdString> {
    Ok(compile::resolve(text).unwrap())
}

#[wasm_bindgen]
pub fn compile(text: &str) -> Result<StdString, StdString> {
    Ok(compile::compile(text).unwrap())
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
