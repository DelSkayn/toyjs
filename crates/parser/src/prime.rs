use ast::{Expr, List, NodeId, ObjectLiteral, PrimeExpr, PropertyDefinition, PropertyName};
use common::{
    span::Span,
    string::{Ascii, Encoding},
};
use lexer::State;
use token::t;

use crate::{expect, next_expect, peek_expect, unexpected, Parser, Result};

static YIELD_STR: &Ascii = Ascii::const_from_str("yield");
static AWAIT_STR: &Ascii = Ascii::const_from_str("await");

impl<'a> Parser<'a> {
    pub(crate) fn parse_prime(&mut self) -> Result<NodeId<PrimeExpr>> {
        self.with_lexer_state(lexer::State::Regex, |this| {
            let token = next_expect!(
                this, "ident", "num", "string", "true", "false", "regex", "null", "this", "{", "[",
                "("
            );

            match token.kind() {
                t!("ident") => {
                    let id = this.push(
                        PrimeExpr::Ident(token.kind_and_data.data_id().unwrap()),
                        token.span,
                    );
                    Ok(id)
                }
                t!("num") => {
                    let id = this.push(
                        PrimeExpr::Number(token.kind_and_data.data_id().unwrap()),
                        token.span,
                    );
                    Ok(id)
                }
                t!("string") | t!("``") => {
                    let id = this.push(
                        PrimeExpr::String(token.kind_and_data.data_id().unwrap()),
                        token.span,
                    );
                    Ok(id)
                }
                t!("true") => {
                    let id = this.push(PrimeExpr::Boolean(true), token.span);
                    Ok(id)
                }
                t!("false") => {
                    let id = this.push(PrimeExpr::Boolean(false), token.span);
                    Ok(id)
                }
                t!("regex") => {
                    let id = this.push(
                        PrimeExpr::Regex(token.kind_and_data.data_id().unwrap()),
                        token.span,
                    );
                    Ok(id)
                }
                t!("null") => {
                    let id = this.push(PrimeExpr::Null, token.span);
                    Ok(id)
                }
                t!("this") => {
                    let id = this.push(PrimeExpr::This, token.span);
                    Ok(id)
                }
                t!("{") => this.with_lexer_state(State::Base, |this| {
                    let id = this.parse_object_literal()?;
                    Ok(this
                        .ast
                        .push_node(PrimeExpr::Object(id), token.span.covers(this.last_span())))
                }),
                t!("[") => this.with_lexer_state(State::Base, |this| this.parse_array_literal()),
                t!("function") => {
                    todo!("function expression")
                }
                t!("(") => {
                    let expression = this.parse_expr()?;
                    let span = token.span.covers(this.last_span());
                    if let Some(t!("=>")) = this.peek_kind() {
                        return this.reparse_arrow_function(expression);
                    }
                    let id = this.push(PrimeExpr::Covered(expression), span);
                    Ok(id)
                }
                t!("yield") if this.state.yield_ident => {
                    let str = this.lexer.data.push_string(Encoding::Ascii(YIELD_STR));
                    let id = this.push(PrimeExpr::Ident(str), token.span);
                    Ok(id)
                }
                t!("await") if this.state.await_ident => {
                    let str = this.lexer.data.push_string(Encoding::Ascii(AWAIT_STR));
                    let id = this.push(PrimeExpr::Ident(str), token.span);
                    Ok(id)
                }
                x => {
                    unexpected!(
                        this, x, "ident", "num", "string", "true", "false", "regex", "null",
                        "this", "{", "[", "("
                    )
                }
            }
        })
    }

    fn parse_object_literal(&mut self) -> Result<ObjectLiteral> {
        let token = next_expect!(self);
        if let t!("}") = token.kind() {
            return Ok(ObjectLiteral::Empty);
        }
        let property = self.parse_property_definition()?;
        let mut last = self.ast.push_node(
            List {
                item: property,
                next: None,
            },
            Span::empty(),
        );
        let res = ObjectLiteral::Item(last);
        loop {
            let token = next_expect!(self, ",", "}");
            match token.kind() {
                t!("}") => break,
                t!(",") => {
                    if let Some(t!("}")) = self.peek_kind() {
                        break;
                    }
                    let property = self.parse_property_definition()?;
                    let new = self.ast.push_node(
                        List {
                            item: property,
                            next: None,
                        },
                        Span::empty(),
                    );
                    self.ast[last].next = Some(new);
                    last = new;
                }
                x => {
                    unexpected!(self, x, ",", "}")
                }
            }
        }
        Ok(res)
    }

    fn parse_property_definition(&mut self) -> Result<NodeId<PropertyDefinition>> {
        let token = peek_expect!(self);
        let name = match token.kind() {
            t!("...") => {
                let expr = self.parse_assignment_expr()?;
                let span = token.span.covers(self.last_span());
                let id = self.ast.push_node(PropertyDefinition::Rest(expr), span);
                return Ok(id);
            }
            t!("*") => {
                todo!("parse generator method")
            }
            _ => self.parse_property_name()?,
        };

        match self.peek_kind() {
            Some(t!(":")) => {
                self.next();
                let expr = self.parse_assignment_expr()?;
                let span = token.span.covers(self.last_span());
                let id = self.ast.push_node(
                    PropertyDefinition::Define {
                        property: name,
                        expr,
                    },
                    span,
                );
                Ok(id)
            }
            Some(t!("=")) => {
                if let PropertyName::Ident(x) = name {
                    self.next();
                    let expr = self.parse_assignment_expr()?;
                    let span = token.span.covers(self.last_span());
                    let id = self.ast.push_node(
                        PropertyDefinition::Covered {
                            ident: x,
                            initializer: expr,
                        },
                        span,
                    );
                    Ok(id)
                } else {
                    unexpected!(self, t!("="), ":")
                }
            }
            x => {
                if let PropertyName::Ident(x) = name {
                    let id = self.ast.push_node(PropertyDefinition::Ident(x), token.span);
                    Ok(id)
                } else if let Some(x) = x {
                    unexpected!(self, x, ":")
                } else {
                    return Err(crate::Error {
                        kind: crate::error::ErrorKind::UnexpectedEnd {
                            expected: vec![t!(":")],
                        },
                        origin: token.span,
                    });
                }
            }
        }
    }

    fn parse_property_name(&mut self) -> Result<PropertyName> {
        let token = next_expect!(self, "ident", "[", "string", "num");
        match token.kind() {
            t!("ident") => Ok(PropertyName::Ident(token.data_id().unwrap())),
            t!("string") => Ok(PropertyName::String(token.data_id().unwrap())),
            t!("num") => Ok(PropertyName::Number(token.data_id().unwrap())),
            t!("[") => {
                let expr = self.parse_assignment_expr()?;
                expect!(self, "]");
                Ok(PropertyName::Computed(expr))
            }
            x => {
                unexpected!(self, x, "ident", "[")
            }
        }
    }

    fn parse_array_literal(&mut self) -> Result<NodeId<PrimeExpr>> {
        todo!()
    }

    fn reparse_arrow_function(&mut self, _expr: NodeId<Expr>) -> Result<NodeId<PrimeExpr>> {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use crate::create_test_parser;

    #[test]
    fn basic_string() {
        create_test_parser!("\"hello world\"", parser);
        match parser.parse_prime() {
            Ok(x) => {
                assert!(matches!(parser.ast[x], ast::PrimeExpr::String(_)))
            }
            Err(_) => {
                panic!()
            }
        }
    }

    #[test]
    fn basic_number() {
        create_test_parser!("3", parser);
        match parser.parse_prime() {
            Ok(x) => {
                assert!(matches!(parser.ast[x], ast::PrimeExpr::Number(_)))
            }
            Err(_) => {
                panic!()
            }
        }
    }

    #[test]
    fn basic_ident() {
        create_test_parser!("hello", parser);
        match parser.parse_prime() {
            Ok(x) => {
                assert!(matches!(parser.ast[x], ast::PrimeExpr::Ident(_)))
            }
            Err(_) => {
                panic!()
            }
        }
    }
}
