use ast::{Class, ClassMember, FunctionKind, NodeId, PropertyName};
use common::string::String;
use token::t;

use crate::{
    expect,
    function::{parse_function, parse_getter, parse_setter, FunctionCtx},
    prime::{is_property_name, parse_property_name},
    Parser, Result,
};

pub fn parse_class(parser: &mut Parser, stmt: bool) -> Result<NodeId<Class>> {
    expect!(parser, "class");
    let name = if stmt {
        Some(parser.parse()?)
    } else {
        match parser.peek_kind() {
            t!("extends") | t!("{") => None,
            _ => Some(parser.parse()?),
        }
    };
    let heritage = parser
        .eat(t!("extends"))
        .then(|| parser.parse())
        .transpose()?;

    expect!(parser, "{");
    let mut head = None;
    let mut cur = None;
    while !parser.eat(t!("}")) {
        let is_static = parser.eat(t!("static"));

        let property = match parser.peek_kind() {
            t!("get") => {
                parser.next();
                if is_property_name(parser.peek_kind()) {
                    let property = parse_property_name(parser)?;
                    let func = parse_getter(parser)?;
                    let item = parser.push(ClassMember::Getter {
                        is_static,
                        property,
                        func,
                    })?;
                    parser.push_list(&mut head, &mut cur, item)?;
                    continue;
                } else {
                    PropertyName::Ident {
                        name: parser.push(String::new_const("get"))?,
                    }
                }
            }
            t!("set") => {
                parser.next();
                if is_property_name(parser.peek_kind()) {
                    let property = parse_property_name(parser)?;
                    let func = parse_setter(parser)?;
                    let item = parser.push(ClassMember::Setter {
                        is_static,
                        property,
                        func,
                    })?;
                    parser.push_list(&mut head, &mut cur, item)?;
                    continue;
                } else {
                    PropertyName::Ident {
                        name: parser.push(String::new_const("get"))?,
                    }
                }
            }
            t!("async") => {
                parser.next();
                let kind = if parser.eat(t!("*")) {
                    FunctionKind::AsyncGenerator
                } else {
                    FunctionKind::Async
                };
                parser.no_line_terminator()?;
                let property = parse_property_name(parser)?;
                let func = parse_function(parser, FunctionCtx::Method, kind)?;
                let item = parser.push(ClassMember::Method {
                    is_static,
                    property,
                    func,
                })?;
                parser.push_list(&mut head, &mut cur, item)?;
                continue;
            }
            t!("*") => {
                parser.next();
                parser.peek();
                parser.no_line_terminator()?;
                let property = parse_property_name(parser)?;
                let func = parse_function(parser, FunctionCtx::Method, FunctionKind::Generator)?;
                let item = parser.push(ClassMember::Method {
                    is_static,
                    property,
                    func,
                })?;
                parser.push_list(&mut head, &mut cur, item)?;
                continue;
            }
            _ => parse_property_name(parser)?,
        };

        let item = match parser.peek_kind() {
            t!("(") => {
                let func = parse_function(parser, FunctionCtx::Method, FunctionKind::Simple)?;
                parser.push(ClassMember::Method {
                    is_static,
                    property,
                    func,
                })?
            }
            t!("=") => {
                parser.next();
                let initializer = Some(parser.parse()?);
                parser.push(ClassMember::Field {
                    is_static,
                    property,
                    initializer,
                })?
            }
            _ => {
                parser.semicolon()?;
                parser.push(ClassMember::Field {
                    is_static,
                    property,
                    initializer: None,
                })?
            }
        };
        parser.push_list(&mut head, &mut cur, item)?;
    }

    Ok(parser.push(Class {
        name,
        heritage,
        body: head,
    })?)
}
