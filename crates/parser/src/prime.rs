use ast::{
    ArrayLiteralEntry, ArrowFunctionBody, AssignOp, BinaryOp, BindingElement, BindingPattern,
    BindingProperty, Expr, Function, FunctionKind, IdentOrPattern, NodeId, NodeList, NodeListId,
    ObjectLiteral, OptionNodeList, PrimeExpr, PropertyDefinition, PropertyName, Symbol, Template,
};
use common::{
    span::Span,
    string::{Ascii, String},
};
use token::{t, TokenKind};

use crate::{
    alter_state,
    binding::parse_ident_name,
    class::parse_class,
    expect,
    function::{parse_function, parse_getter, parse_setter, FunctionCtx},
    is_strict_directive, unexpected, Error, ErrorKind, Parser, ParserState, Result,
};

static YIELD_STR: &Ascii = Ascii::const_from_str("yield");
static AWAIT_STR: &Ascii = Ascii::const_from_str("await");

/// Parsers a primary expression or a expression without any operators.
pub(crate) fn parse_prime(parser: &mut Parser) -> Result<(NodeId<PrimeExpr>, Option<Span>)> {
    let token = parser.peek();
    match token.kind {
        t!("ident") => {
            let symbol = parser.parse()?;
            if let t!("=>") = parser.peek_kind() {
                parser.no_line_terminator()?;
                parser.next();
                let param = parser.push(BindingElement::SingleName {
                    symbol,
                    initializer: None,
                });
                let params = Some(parser.push(NodeList {
                    item: param,
                    next: None,
                }));
                let function = parse_arrow_function(parser, params, None, FunctionKind::Simple)?;
                let id = parser.push(PrimeExpr::Function { function });
                Ok((id, None))
            } else {
                let id = parser.push(PrimeExpr::Ident { symbol });
                Ok((id, None))
            }
        }
        t!("123") => {
            parser.next();
            let id = parser.push(PrimeExpr::Number {
                value: token.data.unwrap().entype(),
            });
            Ok((id, None))
        }
        t!("string") | t!("``") => {
            parser.next();
            let id = parser.push(PrimeExpr::String {
                value: token.data.unwrap().entype(),
            });
            Ok((id, None))
        }
        t!("` ${") => {
            let template = parse_template(parser)?;
            let id = parser.push(PrimeExpr::Template { template });
            Ok((id, None))
        }
        t!("true") => {
            parser.next();
            let id = parser.push(PrimeExpr::Boolean { value: true });
            Ok((id, None))
        }
        t!("false") => {
            parser.next();
            let id = parser.push(PrimeExpr::Boolean { value: false });
            Ok((id, None))
        }
        t!("/") | t!("/=") => {
            parser.next();
            let token = parser.lexer.relex_regex(token);
            if let token::TokenKind::Unknown = token.kind {
                return Err(Error::new(ErrorKind::InvalidToken, token.span));
            }
            let id = parser.push(PrimeExpr::Regex {
                regex: token.data.unwrap().entype(),
            });
            Ok((id, None))
        }
        t!("null") => {
            parser.next();
            let id = parser.push(PrimeExpr::Null);
            Ok((id, None))
        }
        t!("this") => {
            parser.next();
            let id = parser.push(PrimeExpr::This);
            Ok((id, None))
        }
        t!("super") => {
            parser.next();
            let id = parser.push(PrimeExpr::Super);
            Ok((id, None))
        }
        t!("{") => {
            parser.next();
            let (object, span) = parse_object_literal(parser)?;
            Ok((parser.push(PrimeExpr::Object { object }), span))
        }
        t!("[") => {
            parser.next();
            let array = parse_array_literal(parser)?;
            Ok((parser.push(PrimeExpr::Array { array }), None))
        }
        t!("function") => {
            parser.next();
            let kind = if parser.eat(t!("*")) {
                FunctionKind::Generator
            } else {
                FunctionKind::Simple
            };
            let function = parse_function(parser, FunctionCtx::Expression, kind)?;
            Ok((parser.push(PrimeExpr::Function { function }), None))
        }
        t!("async") => parse_async_function(parser).map(|x| (x, None)),
        t!("class") => {
            let class = parse_class(parser, false)?;
            Ok((parser.push(PrimeExpr::Class { class }), None))
        }
        t!("(") => {
            parser.next();
            parse_covered_expression(parser).map(|x| (x, None))
        }
        TokenKind::UnreservedKeyword(_) => {
            let symbol = parser.parse()?;
            if let t!("=>") = parser.peek_kind() {
                parser.no_line_terminator()?;
                parser.next();
                let param = parser.push(BindingElement::SingleName {
                    symbol,
                    initializer: None,
                });
                let params = Some(parser.push(NodeList {
                    item: param,
                    next: None,
                }));
                let function = parse_arrow_function(parser, params, None, FunctionKind::Simple)?;
                let id = parser.push(PrimeExpr::Function { function });
                Ok((id, None))
            } else {
                let id = parser.push(PrimeExpr::Ident { symbol });
                Ok((id, None))
            }
        }
        x => {
            unexpected!(
                parser, x, "ident", "123", "string", "true", "false", "regex", "null", "this", "{",
                "[", "(", "function"
            )
        }
    }
}

/// Parses a covered expression, e.g.:
/// ```javascript
/// (/* start here */ 1 + 1, b)
/// ```
/// Also parses arrow function, e.g.:
/// ```javascript
/// (a,b) => a + b
/// ```
/// Refining the covered expression if it is actually a parameter list.
fn parse_covered_expression(parser: &mut Parser) -> Result<NodeId<PrimeExpr>> {
    let mut head = None;
    let mut cur = None;
    let mut rest = None;
    while t!(")") != parser.peek_kind() {
        if parser.eat(t!("...")) {
            rest = Some(parser.parse()?);
            break;
        }
        alter_state!(parser => {
            parser.state.insert(ParserState::In);
            let expr = parser.parse()?;
        });
        parser.push_list(&mut head, &mut cur, expr);
        if !parser.eat(t!(",")) {
            break;
        }
    }
    expect!(parser, ")");

    // spread operator or an empty head always indicate an arrow function.
    if rest.is_some() || head.is_none() {
        parser.peek();
        parser.no_line_terminator()?;
        // TODO: Improve error message.
        expect!(parser, "=>");
        return reparse_arrow_function(parser, head, rest);
    }
    if let t!("=>") = parser.peek_kind() {
        parser.no_line_terminator()?;
        parser.next();
        reparse_arrow_function(parser, head, None)
    } else {
        let Some(expr) = head else {
            unreachable!();
        };
        let id = parser.push(PrimeExpr::Covered { expr });
        Ok(id)
    }
}

/// Parses a template expression, e.g.:
pub fn parse_template(parser: &mut Parser) -> Result<NodeId<Template>> {
    let token = parser.next();
    match token.kind {
        t!("``") => {
            let text = token.data.unwrap().entype();
            Ok(parser.push(Template::Tail { text }))
        }
        t!("` ${") => {
            let expr = parser.parse()?;
            let text = token.data.unwrap().entype();
            let next = parse_template(parser)?;
            Ok(parser.push(Template::Head { text, expr, next }))
        }
        t!("}") => {
            let token = parser.lexer.relex_template(token);
            match token.kind {
                t!("} ${") => {
                    let expr = parser.parse()?;
                    let text = token.data.unwrap().entype();
                    let next = parse_template(parser)?;
                    Ok(parser.push(Template::Head { text, expr, next }))
                }
                t!("} `") => {
                    let text = token.data.unwrap().entype();
                    Ok(parser.push(Template::Tail { text }))
                }
                x => unexpected!(parser, x, "} `"),
            }
        }
        x => unexpected!(parser, x),
    }
}

/// Parses a object literal expression, e.g.:
/// ```javascript
/// { /* start here */ foo: "bar" }
/// ```
///
/// Returns both the parsed object literal as well the span of the first '=' if the object
/// literal contains a covered binding initalizer.
fn parse_object_literal(parser: &mut Parser) -> Result<(ObjectLiteral, Option<Span>)> {
    if let t!("}") = parser.peek_kind() {
        parser.next();
        return Ok((ObjectLiteral::Empty, None));
    }
    let (property, mut span) = parse_property_definition(parser)?;
    let mut last = parser.push(NodeList {
        item: property,
        next: None,
    });
    let res = ObjectLiteral::Item { definition: last };
    loop {
        match parser.peek_kind() {
            t!("}") => {
                parser.next();
                break;
            }
            t!(",") => {
                parser.next();
                if let t!("}") = parser.peek_kind() {
                    parser.next();
                    break;
                }
                let (property, new_span) = parse_property_definition(parser)?;
                span = span.or(new_span);

                let new_last = parser.push(NodeList {
                    item: property,
                    next: None,
                });
                parser[last].next = Some(new_last);
                last = new_last;
            }
            x => {
                unexpected!(parser, x, ",", "}")
            }
        }
    }
    Ok((res, span))
}

/// Returns wether the token can be parsed as a property name.
pub fn is_property_name(kind: TokenKind) -> bool {
    matches!(
        kind,
        t!("ident")
            | TokenKind::Keyword(_)
            | TokenKind::UnreservedKeyword(_)
            | t!("[")
            | t!("string")
            | t!("123"),
    )
}

/// Parses a property definition, e.g. `foo: 1` and `"bar": 2` in :
/// ```javascript
/// { /* start here */ foo: 1, "bar": 2 }
/// ```
fn parse_property_definition(
    parser: &mut Parser,
) -> Result<(NodeId<PropertyDefinition>, Option<Span>)> {
    let token = parser.peek();
    let property = match token.kind {
        t!("...") => {
            parser.next();
            let expr = parser.parse()?;
            let id = parser.push(PropertyDefinition::Rest { rest: expr });
            return Ok((id, None));
        }
        t!("*") => {
            parser.next();
            let property = parse_property_name(parser)?;
            let func = parse_function(parser, FunctionCtx::Method, FunctionKind::Generator)?;
            return Ok((
                parser.push(PropertyDefinition::Method { property, func }),
                None,
            ));
        }
        t!("get") => {
            parser.next();
            if is_property_name(parser.peek_kind()) {
                let property = parse_property_name(parser)?;
                let func = parse_getter(parser)?;
                return Ok((
                    parser.push(PropertyDefinition::Getter { property, func }),
                    None,
                ));
            }
            PropertyName::Ident {
                name: parser.push(String::new_const("get")),
            }
        }
        t!("set") => {
            parser.next();
            if is_property_name(parser.peek_kind()) {
                let property = parse_property_name(parser)?;
                let func = parse_setter(parser)?;
                return Ok((
                    parser.push(PropertyDefinition::Setter { property, func }),
                    None,
                ));
            }
            PropertyName::Ident {
                name: parser.push(String::new_const("set")),
            }
        }
        t!("async") => {
            parser.next();
            if let t!(":") = parser.peek_kind() {
                PropertyName::Ident {
                    name: parser.push(String::new_const("async")),
                }
            } else {
                parser.no_line_terminator()?;
                let kind = if parser.eat(t!("*")) {
                    FunctionKind::AsyncGenerator
                } else {
                    FunctionKind::Generator
                };
                let property = parse_property_name(parser)?;
                let func = parse_function(parser, FunctionCtx::Method, kind)?;
                return Ok((
                    parser.push(PropertyDefinition::Method { property, func }),
                    None,
                ));
            }
        }
        _ => parse_property_name(parser)?,
    };

    match parser.peek_kind() {
        t!(":") => {
            parser.next();
            let expr = parser.parse()?;
            let id = parser.push(PropertyDefinition::Define { property, expr });
            Ok((id, None))
        }
        t!("=") => {
            if let PropertyName::Ident { name } = property {
                let span = parser.next().span;
                let expr = parser.parse()?;
                let symbol = parser.push(Symbol {
                    name,
                    span: token.span,
                });

                let id = parser.push(PropertyDefinition::Covered {
                    symbol,
                    initializer: expr,
                });
                Ok((id, Some(span)))
            } else {
                unexpected!(parser, t!("="), ":")
            }
        }
        t!("(") => {
            let func = parse_function(parser, FunctionCtx::Method, FunctionKind::Simple)?;
            Ok((
                parser.push(PropertyDefinition::Method { property, func }),
                None,
            ))
        }
        x => {
            if let PropertyName::Ident { name } = property {
                let ident = parser.push(Symbol {
                    name,
                    span: token.span,
                });
                let id = parser.push(PropertyDefinition::Ident { ident });
                Ok((id, None))
            } else {
                unexpected!(parser, x, ":")
            }
        }
    }
}

pub fn parse_property_name(parser: &mut Parser) -> Result<PropertyName> {
    let token = parser.peek();
    match token.kind {
        t!("string") => {
            parser.next();
            Ok(PropertyName::String {
                value: token.data.unwrap().entype(),
            })
        }
        t!("123") => {
            parser.next();
            Ok(PropertyName::Number {
                value: token.data.unwrap().entype(),
            })
        }
        t!("[") => {
            parser.next();
            let expr = parser.parse()?;
            expect!(parser, "]");
            Ok(PropertyName::Computed { expr })
        }
        _ => {
            let name = parse_ident_name(parser)?;
            Ok(PropertyName::Ident { name })
        }
    }
}

fn parse_array_literal(parser: &mut Parser) -> Result<Option<NodeListId<ArrayLiteralEntry>>> {
    let mut head = None;
    let mut cur = None;

    loop {
        match parser.peek_kind() {
            t!("]") => {
                break;
            }
            t!(",") => {
                parser.next();
                let node = parser.push(ArrayLiteralEntry {
                    expr: None,
                    is_spread: false,
                });
                parser.push_list(&mut head, &mut cur, node);
            }
            t!("...") => {
                parser.next();
                let expr = parser.parse()?;
                let node = parser.push(ArrayLiteralEntry {
                    expr: Some(expr),
                    is_spread: true,
                });
                parser.push_list(&mut head, &mut cur, node);
                if !parser.eat(t!(",")) {
                    break;
                }
            }
            _ => {
                let expr = parser.parse()?;
                let node = parser.push(ArrayLiteralEntry {
                    expr: Some(expr),
                    is_spread: false,
                });
                parser.push_list(&mut head, &mut cur, node);
                if !parser.eat(t!(",")) {
                    break;
                }
            }
        }
    }
    expect!(parser, "]");
    Ok(head)
}

pub fn reparse_arrow_function(
    parser: &mut Parser,
    expr: Option<NodeListId<Expr>>,
    rest: Option<NodeId<IdentOrPattern>>,
) -> Result<NodeId<PrimeExpr>> {
    let mut head = None;
    let mut prev = None;

    let mut cur: Option<NodeListId<Expr>> = expr.into();
    while let Some(expr) = cur {
        let Some(param) = reparse_binding_element(parser, parser[expr].item)? else {
            unexpected!(parser,t!("=>") => "covered expression can't be parsed as parameters");
        };
        let param = parser.push(param);
        parser.push_list(&mut head, &mut prev, param);
        cur = parser[expr].next;
    }
    let function = parse_arrow_function(parser, head, rest, FunctionKind::Simple)?;
    Ok(parser.push(PrimeExpr::Function { function }))
}

fn reparse_binding_element(
    parser: &mut Parser,
    expr: NodeId<Expr>,
) -> Result<Option<BindingElement>> {
    match parser[expr] {
        Expr::Prime { expr } => match parser[expr] {
            PrimeExpr::Ident { symbol } => Ok(Some(BindingElement::SingleName {
                symbol,
                initializer: None,
            })),
            PrimeExpr::Object { object } => {
                let Some(pattern) = reparse_object_lit(parser, object)? else {
                    return Ok(None);
                };
                let pattern = parser.push(pattern);
                Ok(Some(BindingElement::Pattern {
                    pattern,
                    initializer: None,
                }))
            }
            PrimeExpr::Array { array } => {
                let Some(pattern) = reparse_array_lit(parser, array)? else {
                    return Ok(None);
                };
                let pattern = parser.push(pattern);
                Ok(Some(BindingElement::Pattern {
                    pattern,
                    initializer: None,
                }))
            }
            _ => Ok(None),
        },
        Expr::Binary {
            op: BinaryOp::Assign(AssignOp::Assign),
            left,
            right,
        } => {
            let mut binding = reparse_binding_element(parser, left)?;
            let Some(
                BindingElement::SingleName {
                    ref mut initializer,
                    ..
                }
                | BindingElement::Pattern {
                    ref mut initializer,
                    ..
                },
            ) = binding
            else {
                return Ok(None);
            };
            debug_assert!(initializer.is_none());
            *initializer = Some(right);
            Ok(binding)
        }
        _ => Ok(None),
    }
}

pub fn reparse_object_lit(
    parser: &mut Parser,
    expr: ObjectLiteral,
) -> Result<Option<BindingPattern>> {
    let ObjectLiteral::Item { mut definition } = expr else {
        return Ok(Some(BindingPattern::Object {
            properties: None,
            rest: None,
        }));
    };

    let mut head = None;
    let mut cur = None;
    let mut rest = None;
    loop {
        let item_node = parser[definition].item;
        let prop = match parser[item_node] {
            PropertyDefinition::Ident { ident: symbol } => BindingProperty::Binding {
                symbol,
                initializer: None,
            },

            // TODO: make sure this is a syntax error in actual object literals
            PropertyDefinition::Covered {
                symbol: ident,
                initializer,
            } => BindingProperty::Binding {
                symbol: ident,
                initializer: Some(initializer),
            },
            PropertyDefinition::Define { property, expr } => {
                let Some(element) = reparse_binding_element(parser, expr)? else {
                    return Ok(None);
                };
                let element = parser.push(element);
                BindingProperty::Property {
                    name: property,
                    element,
                }
            }
            PropertyDefinition::Method { .. }
            | PropertyDefinition::Getter { .. }
            | PropertyDefinition::Setter { .. } => return Ok(None),
            PropertyDefinition::Rest { rest: r } => {
                // Rest not last in the object
                if rest.is_some() || parser[definition].next.is_some() {
                    return Ok(None);
                }
                let Expr::Prime { expr } = parser[r] else {
                    return Ok(None);
                };
                let PrimeExpr::Ident { symbol } = parser[expr] else {
                    return Ok(None);
                };
                rest = Some(symbol);
                // rest is last
                break;
            }
        };
        let prop = parser.push(prop);
        parser.push_list(&mut head, &mut cur, prop);

        let Some(next) = parser[definition].next else {
            break;
        };
        definition = next;
    }

    Ok(Some(BindingPattern::Object {
        properties: head,
        rest,
    }))
}

pub fn reparse_array_lit(
    parser: &mut Parser,
    expr: Option<NodeListId<ArrayLiteralEntry>>,
) -> Result<Option<BindingPattern>> {
    let Some(head) = expr else {
        return Ok(Some(BindingPattern::Array {
            elements: None,
            rest: None,
        }));
    };

    let mut cur = Some(head);
    let mut head = None;
    let mut prev = None;
    let mut rest = None;

    while let Some(item) = parser.next_list(&mut cur) {
        let element = if let Some(x) = parser[item].expr {
            if parser[item].is_spread {
                if cur.is_some() {
                    return Ok(None);
                } else {
                    let Some(elem) = reparse_ident_or_pattern(parser, x)? else {
                        return Ok(None);
                    };
                    rest = Some(parser.push(elem));
                    break;
                }
            } else {
                let Some(elem) = reparse_binding_element(parser, x)? else {
                    return Ok(None);
                };
                Some(parser.push(elem))
            }
        } else {
            None
        };
        prev = Some(parser.push(OptionNodeList {
            item: element,
            next: prev,
        }));
        head = head.or(prev);
    }

    Ok(Some(BindingPattern::Array {
        elements: head,
        rest,
    }))
}

fn reparse_ident_or_pattern(
    parser: &mut Parser,
    expr: NodeId<Expr>,
) -> Result<Option<IdentOrPattern>> {
    let Expr::Prime { expr } = parser[expr] else {
        return Ok(None);
    };
    match parser[expr] {
        PrimeExpr::Ident { symbol } => Ok(Some(IdentOrPattern::Ident { name: symbol })),
        PrimeExpr::Object { object } => {
            let Some(pat) = reparse_object_lit(parser, object)? else {
                return Ok(None);
            };
            let pattern = parser.push(pat);
            Ok(Some(IdentOrPattern::Pattern { pattern }))
        }
        PrimeExpr::Array { array } => {
            let Some(pat) = reparse_array_lit(parser, array)? else {
                return Ok(None);
            };
            let pattern = parser.push(pat);
            Ok(Some(IdentOrPattern::Pattern { pattern }))
        }
        _ => Ok(None),
    }
}

fn parse_arrow_function(
    parser: &mut Parser,
    params: Option<NodeListId<BindingElement>>,
    rest_param: Option<NodeId<IdentOrPattern>>,
    kind: FunctionKind,
) -> Result<NodeId<Function>> {
    let state = parser.state;

    parser.state.set(
        ParserState::AwaitIdent,
        matches!(kind, FunctionKind::Simple | FunctionKind::Generator),
    );
    let body = if let t!("{") = parser.peek_kind() {
        parser.next();
        let mut head = None;
        let mut cur = None;

        while !parser.eat(t!("}")) {
            let stmt = parser.parse()?;
            if cur.is_none()
                && !parser.state.contains(ParserState::Strict)
                && is_strict_directive(parser, stmt)
            {
                parser.state.insert(ParserState::Strict);
            }
            parser.push_list(&mut head, &mut cur, stmt);
        }
        ArrowFunctionBody::Stmt { body: head }
    } else {
        ArrowFunctionBody::Expr {
            expr: parser.parse()?,
        }
    };

    let is_strict = state.contains(ParserState::Strict);
    parser.state = state;
    Ok(parser.push(Function::Arrow {
        is_strict,
        kind,
        params,
        rest_param,
        body,
    }))
}

fn parse_async_function(parser: &mut Parser) -> Result<NodeId<PrimeExpr>> {
    parser.next();
    let token = parser.peek_kind();
    parser.no_line_terminator()?;
    // TODO not sure if this the right way to do it.
    match token {
        t!("function") => {
            parser.next();
            let kind = if parser.eat(t!("*")) {
                FunctionKind::AsyncGenerator
            } else {
                FunctionKind::Async
            };
            let function = parse_function(parser, FunctionCtx::Expression, kind)?;
            Ok(parser.push(PrimeExpr::Function { function }))
        }
        t!("(") => {
            parser.next();
            let mut head = None;
            let mut cur = None;
            let mut rest = None;
            while !parser.eat(t!(")")) {
                if parser.eat(t!("...")) {
                    rest = Some(parser.parse()?);
                } else {
                    let element = parser.parse()?;
                    parser.push_list(&mut head, &mut cur, element);
                }
            }
            parser.peek();
            // TODO: Improve error her if no => is found
            parser.no_line_terminator()?;
            expect!(parser, "=>");
            let function = parse_arrow_function(parser, head, rest, FunctionKind::Async)?;
            Ok(parser.push(PrimeExpr::Function { function }))
        }
        _ => {
            let symbol = parser.parse()?;
            let element = parser.push(BindingElement::SingleName {
                symbol,
                initializer: None,
            });
            let element = Some(parser.push(NodeList {
                item: element,
                next: None,
            }));
            parser.peek();
            // TODO: Improve error her if no => is found
            parser.no_line_terminator()?;
            expect!(parser, "=>");
            let function = parse_arrow_function(parser, element, None, FunctionKind::Async)?;
            Ok(parser.push(PrimeExpr::Function { function }))
        }
    }
}

#[cfg(test)]
mod test {
    use ast::{ObjectLiteral, PrimeExpr, PropertyDefinition};

    use crate::{create_test_parser, prime::parse_prime};

    #[test]
    fn basic_string() {
        create_test_parser!("\"hello world\"", parser);
        match parse_prime(&mut parser) {
            Ok((x, _)) => {
                assert!(matches!(parser[x], ast::PrimeExpr::String { .. }))
            }
            Err(_) => {
                panic!()
            }
        }
    }

    #[test]
    fn basic_number() {
        create_test_parser!("3", parser);
        match parse_prime(&mut parser) {
            Ok((x, _)) => {
                assert!(matches!(parser[x], ast::PrimeExpr::Number { .. }))
            }
            Err(_) => {
                panic!()
            }
        }
    }

    #[test]
    fn basic_ident() {
        create_test_parser!("hello", parser);
        match parse_prime(&mut parser) {
            Ok((x, _)) => {
                assert!(matches!(parser[x], ast::PrimeExpr::Ident { .. }))
            }
            Err(_) => {
                panic!()
            }
        }
    }

    #[test]
    fn basic_object() {
        create_test_parser!("{}", parser);
        match parse_prime(&mut parser) {
            Ok((x, _)) => {
                assert!(matches!(
                    parser[x],
                    ast::PrimeExpr::Object {
                        object: ast::ObjectLiteral::Empty
                    }
                ))
            }
            Err(_) => {
                panic!()
            }
        }
    }

    #[test]
    fn object_with_identifier() {
        create_test_parser!("{ hello }", parser);
        match parse_prime(&mut parser) {
            Ok((x, _)) => {
                let PrimeExpr::Object { object } = parser[x] else {
                    panic!("not object");
                };
                let ObjectLiteral::Item { definition } = object else {
                    panic!("object empty");
                };
                let item = parser[definition].item;
                assert!(matches!(parser[item], PropertyDefinition::Ident { .. }));
            }
            Err(_) => {
                panic!()
            }
        }
    }

    #[test]
    fn basic_array() {
        create_test_parser!("[]", parser);
        match parse_prime(&mut parser) {
            Ok((x, _)) => {
                assert!(matches!(parser[x], ast::PrimeExpr::Array { .. }))
            }
            Err(_) => {
                panic!()
            }
        }
    }

    #[test]
    fn object_literal_covered() {
        create_test_parser!("{ a = 3 }", parser);
        match parse_prime(&mut parser) {
            Ok((x, span)) => {
                assert!(span.is_some());
                assert!(matches!(parser[x], ast::PrimeExpr::Object { .. }))
            }
            Err(_) => {
                panic!()
            }
        }
    }
}
