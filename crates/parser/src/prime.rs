use ast::{
    ArrayLiteral, Expr, ListId, NodeId, NodeList, ObjectLiteral, PrimeExpr, PropertyDefinition,
    PropertyName,
};
use common::string::{Ascii, Encoding};
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
                "(", "function"
            );

            match token.kind() {
                t!("ident") => {
                    let id = this.push(PrimeExpr::Ident(token.kind_and_data.data_id().unwrap()));
                    Ok(id)
                }
                t!("num") => {
                    let id = this.push(PrimeExpr::Number(token.kind_and_data.data_id().unwrap()));
                    Ok(id)
                }
                t!("string") | t!("``") => {
                    let id = this.push(PrimeExpr::String(token.kind_and_data.data_id().unwrap()));
                    Ok(id)
                }
                t!("true") => {
                    let id = this.push(PrimeExpr::Boolean(true));
                    Ok(id)
                }
                t!("false") => {
                    let id = this.push(PrimeExpr::Boolean(false));
                    Ok(id)
                }
                t!("regex") => {
                    let id = this.push(PrimeExpr::Regex(token.kind_and_data.data_id().unwrap()));
                    Ok(id)
                }
                t!("null") => {
                    let id = this.push(PrimeExpr::Null);
                    Ok(id)
                }
                t!("this") => {
                    let id = this.push(PrimeExpr::This);
                    Ok(id)
                }
                t!("{") => this.with_lexer_state(State::Base, |this| {
                    let id = this.parse_object_literal()?;
                    Ok(this.ast.push_node(PrimeExpr::Object(id)))
                }),
                t!("[") => this.with_lexer_state(State::Base, |this| {
                    let array = this.parse_array_literal()?;
                    Ok(this.ast.push_node(PrimeExpr::Array(array)))
                }),
                t!("function") => {
                    todo!("function expression")
                }
                t!("(") => {
                    let expression = this.parse_expr()?;
                    expect!(this, ")");
                    if this.ast[expression].next.is_some() {
                        peek_expect!(this, "=>");
                        this.reparse_arrow_function(expression)
                    } else if let Some(t!("=>")) = this.peek_kind() {
                        this.reparse_arrow_function(expression)
                    } else {
                        let expr = this.ast[expression].item;
                        let id = this.push(PrimeExpr::Covered(expr));
                        Ok(id)
                    }
                }
                t!("yield") if this.state.yield_ident => {
                    let str = this.lexer.data.push_string(Encoding::Ascii(YIELD_STR));
                    let id = this.push(PrimeExpr::Ident(str));
                    Ok(id)
                }
                t!("await") if this.state.await_ident => {
                    let str = this.lexer.data.push_string(Encoding::Ascii(AWAIT_STR));
                    let id = this.push(PrimeExpr::Ident(str));
                    Ok(id)
                }
                x => {
                    unexpected!(
                        this, x, "ident", "num", "string", "true", "false", "regex", "null",
                        "this", "{", "[", "(", "function"
                    )
                }
            }
        })
    }

    fn parse_object_literal(&mut self) -> Result<ObjectLiteral> {
        let token = peek_expect!(self);
        if let t!("}") = token.kind() {
            self.next();
            return Ok(ObjectLiteral::Empty);
        }
        let property = self.parse_property_definition()?;
        let mut last = self.ast.append_list(property, None);
        let res = ObjectLiteral::Item(last);
        loop {
            let token = peek_expect!(self, ",", "}");
            match token.kind() {
                t!("}") => {
                    self.next();
                    break;
                }
                t!(",") => {
                    self.next();
                    if let Some(t!("}")) = self.peek_kind() {
                        break;
                    }
                    let property = self.parse_property_definition()?;
                    last = self.ast.append_list(property, Some(last));
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
                let id = self.ast.push_node(PropertyDefinition::Rest(expr));
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
                let id = self.ast.push_node(PropertyDefinition::Define {
                    property: name,
                    expr,
                });
                Ok(id)
            }
            Some(t!("=")) => {
                if let PropertyName::Ident(x) = name {
                    self.next();
                    let expr = self.parse_assignment_expr()?;
                    let id = self.ast.push_node(PropertyDefinition::Covered {
                        ident: x,
                        initializer: expr,
                    });
                    Ok(id)
                } else {
                    unexpected!(self, t!("="), ":")
                }
            }
            x => {
                if let PropertyName::Ident(x) = name {
                    let id = self.ast.push_node(PropertyDefinition::Ident(x));
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

    fn parse_array_literal(&mut self) -> Result<NodeId<NodeList<ArrayLiteral>>> {
        let res = self.ast.append_node_list(
            ArrayLiteral {
                expr: None,
                is_spread: false,
            },
            None,
        );

        let token = peek_expect!(self);
        if let t!("]") = token.kind() {
            return Ok(res);
        } else if let t!("...") = token.kind() {
            self.ast[res].data.expr = Some(self.parse_assignment_expr()?);
            self.ast[res].data.is_spread = true;
        } else if t!(",") != token.kind() {
            self.ast[res].data.expr = Some(self.parse_assignment_expr()?);
        }

        let mut item = self.ast.append_node_list(
            ArrayLiteral {
                expr: None,
                is_spread: false,
            },
            Some(res),
        );
        loop {
            let token = next_expect!(self);
            match token.kind() {
                t!("]") => return Ok(res),
                t!(",") => {
                    item = self.ast.append_node_list(
                        ArrayLiteral {
                            expr: None,
                            is_spread: false,
                        },
                        Some(item),
                    );
                }
                t!("...") => {
                    let expr = self.parse_assignment_expr()?;
                    self.ast[item].data.expr = Some(expr);
                    self.ast[item].data.is_spread = true;
                    item = self.ast.append_node_list(
                        ArrayLiteral {
                            expr: None,
                            is_spread: false,
                        },
                        Some(item),
                    );
                }
                _ => {
                    let expr = self.parse_assignment_expr()?;
                    self.ast[item].data.expr = Some(expr);
                    item = self.ast.append_node_list(
                        ArrayLiteral {
                            expr: None,
                            is_spread: false,
                        },
                        Some(item),
                    );
                }
            }
        }
    }

    fn reparse_arrow_function(&mut self, _expr: ListId<Expr>) -> Result<NodeId<PrimeExpr>> {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use ast::{ObjectLiteral, PrimeExpr, PropertyDefinition};

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

    #[test]
    fn basic_object() {
        create_test_parser!("{}", parser);
        match parser.parse_prime() {
            Ok(x) => {
                assert!(matches!(
                    parser.ast[x],
                    ast::PrimeExpr::Object(ast::ObjectLiteral::Empty)
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
        match parser.parse_prime() {
            Ok(x) => {
                let PrimeExpr::Object(ObjectLiteral::Item(node)) = parser.ast[x] else {
                    panic!("not object");
                };
                let item = parser.ast[node].item;
                assert!(matches!(parser.ast[item], PropertyDefinition::Ident(_)));
            }
            Err(_) => {
                panic!()
            }
        }
    }

    #[test]
    fn basic_array() {
        create_test_parser!("[]", parser);
        match parser.parse_prime() {
            Ok(x) => {
                assert!(matches!(parser.ast[x], ast::PrimeExpr::Array(_)))
            }
            Err(_) => {
                panic!()
            }
        }
    }
}
