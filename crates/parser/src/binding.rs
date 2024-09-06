use ast::{
    BindingElement, BindingPattern, BindingProperty, IdentOrPattern, NodeId, PropertyName, Symbol,
};
use common::string::String;
use token::{t, TokenKind};

use crate::{expect, unexpected, Parse, Parser, ParserState, Result};

impl Parse for IdentOrPattern {
    fn parse(parser: &mut Parser) -> Result<NodeId<IdentOrPattern>> {
        match parser.peek_kind() {
            t!("{") | t!("[") => {
                let pattern = parser.parse()?;
                Ok(parser.push(IdentOrPattern::Pattern { pattern }))
            }
            _ => {
                let name = parser.parse()?;
                Ok(parser.push(IdentOrPattern::Ident { name }))
            }
        }
    }
}

impl Parse for BindingPattern {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        match parser.next().kind {
            t!("{") => parse_object_pattern(parser),
            t!("[") => parse_array_pattern(parser),
            x => unexpected!(parser, x, "{", "["),
        }
    }
}

pub fn parse_ident_name(parser: &mut Parser) -> Result<NodeId<String>> {
    let next = parser.next();
    match next.kind {
        TokenKind::Ident => return Ok(next.data.unwrap().entype()),
        TokenKind::Keyword(x) => return Ok(parser.push(x.to_string())),
        TokenKind::UnreservedKeyword(x) => return Ok(parser.push(x.to_string())),
        x => unexpected!(parser, x, "ident"),
    }
}

impl Parse for Symbol {
    fn parse(parser: &mut Parser) -> Result<NodeId<Symbol>> {
        let name = parse_ident(parser)?;
        let span = *parser.last_span();
        let res = parser.push(Symbol { name, span });
        Ok(res)
    }
}

pub fn parse_ident(parser: &mut Parser) -> Result<NodeId<String>> {
    let next = parser.next();
    match next.kind {
        t!("ident") => Ok(next.data.unwrap().entype()),
        t!("yield") => {
            if parser.state.contains(ParserState::YieldIdent) {
                Ok(parser.push(String::new_const("yield")))
            } else {
                unexpected!(parser,t!("yield"),"ident" => "not allowed as an identifier in this context");
            }
        }
        t!("await") => {
            if parser.state.contains(ParserState::AwaitIdent) {
                Ok(parser.push(String::new_const("await")))
            } else {
                unexpected!(parser,t!("await"),"ident" => "not allowed as an identifier in this context");
            }
        }
        t!("let") => {
            if parser.state.contains(ParserState::Strict) {
                unexpected!(parser,t!("let"),"ident" => "not allowed as an identifier in strict mode");
            } else {
                Ok(parser.push(String::new_const("let")))
            }
        }
        t!("static") => {
            if parser.state.contains(ParserState::Strict) {
                unexpected!(parser,t!("static"),"ident" => "not allowed as an identifier in strict mode");
            } else {
                Ok(parser.push(String::new_const("static")))
            }
        }
        t!("implements") => {
            if parser.state.contains(ParserState::Strict) {
                unexpected!(parser,t!("implements"),"ident" => "not allowed as an identifier in strict mode");
            } else {
                Ok(parser.push(String::new_const("implements")))
            }
        }
        t!("interface") => {
            if parser.state.contains(ParserState::Strict) {
                unexpected!(parser,t!("interface"),"ident" => "not allowed as an identifier in strict mode");
            } else {
                Ok(parser.push(String::new_const("interface")))
            }
        }
        t!("package") => {
            if parser.state.contains(ParserState::Strict) {
                unexpected!(parser,t!("package"),"ident" => "not allowed as an identifier in strict mode");
            } else {
                Ok(parser.push(String::new_const("package")))
            }
        }
        t!("private") => {
            if parser.state.contains(ParserState::Strict) {
                unexpected!(parser,t!("private"),"ident" => "not allowed as an identifier in strict mode");
            } else {
                Ok(parser.push(String::new_const("private")))
            }
        }
        t!("protected") => {
            if parser.state.contains(ParserState::Strict) {
                unexpected!(parser,t!("protected"),"ident" => "not allowed as an identifier in strict mode");
            } else {
                Ok(parser.push(String::new_const("protected")))
            }
        }
        t!("public") => {
            if parser.state.contains(ParserState::Strict) {
                unexpected!(parser,t!("public"),"ident" => "not allowed as an identifier in strict mode");
            } else {
                Ok(parser.push(String::new_const("public")))
            }
        }
        // Next keywords are always allowed as identifiers.
        TokenKind::UnreservedKeyword(x) => Ok(parser.push(x.to_string())),
        x => unexpected!(parser, x, "ident"),
    }
}

pub fn parse_object_pattern(parser: &mut Parser) -> Result<NodeId<BindingPattern>> {
    let mut head = None;
    let mut cur = None;
    let mut rest = None;
    loop {
        match parser.peek_kind() {
            t!("}") => {
                parser.next();
                break;
            }
            t!("...") => {
                parser.next();
                let symbol = parser.parse()?;

                expect!(parser, "}");
                rest = Some(symbol);
                break;
            }
            _ => {
                let prop = parse_binding_property(parser)?;
                let item = parser.push(prop);
                parser.push_list(&mut head, &mut cur, item);
                head = head.or(cur.into());
            }
        }
        if !parser.eat(t!(",")) {
            expect!(parser, "}");
            break;
        }
    }

    Ok(parser.push(BindingPattern::Object {
        properties: head,
        rest,
    }))
}

pub fn parse_binding_property(parser: &mut Parser) -> Result<BindingProperty> {
    let next = parser.peek();
    match next.kind {
        t!("string") => {
            parser.next();
            expect!(parser, ":");
            let name = PropertyName::String {
                value: next.data.unwrap().entype(),
            };
            let element = parser.parse()?;
            Ok(BindingProperty::Property { name, element })
        }
        t!("123") => {
            parser.next();
            expect!(parser, ":");
            let name = PropertyName::Number {
                value: next.data.unwrap().entype(),
            };
            let element = parse_binding_element(parser)?;
            let element = parser.push(element);
            Ok(BindingProperty::Property { name, element })
        }
        t!("[") => {
            parser.next();
            let expr = parser.parse()?;
            expect!(parser, "]");
            expect!(parser, ":");
            let name = PropertyName::Computed { expr };
            let element = parser.parse()?;
            Ok(BindingProperty::Property { name, element })
        }
        _ => {
            let name = parse_ident_name(parser)?;
            let span = *parser.last_span();
            if parser.eat(t!(":")) {
                let name = PropertyName::Ident { name };
                let element = parser.parse()?;
                Ok(BindingProperty::Property { name, element })
            } else {
                let initializer = parser.eat(t!("=")).then(|| parser.parse()).transpose()?;

                let symbol = parser.push(Symbol { name, span });

                Ok(BindingProperty::Binding {
                    symbol,
                    initializer,
                })
            }
        }
    }
}

pub fn parse_array_pattern(parser: &mut Parser) -> Result<NodeId<BindingPattern>> {
    let mut head = None;
    let mut cur = None;
    let mut rest = None;
    loop {
        match parser.peek_kind() {
            t!("]") => {
                break;
            }
            t!("...") => {
                parser.next();
                rest = Some(parser.parse()?);
                break;
            }
            t!(",") => {
                parser.next();
                parser.push_option_list(&mut head, &mut cur, None);
            }
            _ => {
                let item = parser.parse()?;
                parser.push_option_list(&mut head, &mut cur, Some(item));
                if !parser.eat(t!(",")) {
                    break;
                }
            }
        }
    }
    expect!(parser, "]");

    Ok(parser.push(BindingPattern::Array {
        elements: head,
        rest,
    }))
}

impl Parse for BindingElement {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let p = parse_binding_element(parser)?;
        Ok(parser.push(p))
    }
}

pub fn parse_binding_element(parser: &mut Parser) -> Result<BindingElement> {
    match parser.peek_kind() {
        t!("{") | t!("[") => {
            let pattern = parser.parse()?;
            let initializer = parser.eat(t!("=")).then(|| parser.parse()).transpose()?;
            Ok(BindingElement::Pattern {
                pattern,
                initializer,
            })
        }
        _ => {
            let symbol = parser.parse()?;
            let initializer = parser.eat(t!("=")).then(|| parser.parse()).transpose()?;
            Ok(BindingElement::SingleName {
                symbol,
                initializer,
            })
        }
    }
}
