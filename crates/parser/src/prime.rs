use ast::{
    ArrayLiteral, Expr, ListId, NodeId, ObjectLiteral, PrimeExpr, PropertyDefinition, PropertyName,
};
use common::string::Ascii;
use token::{t, TokenKind};

use crate::{expect, peek_expect, unexpected, Parser, Result};

static YIELD_STR: &Ascii = Ascii::const_from_str("yield");
static AWAIT_STR: &Ascii = Ascii::const_from_str("await");

impl<'a> Parser<'a> {
    pub(crate) fn parse_prime(&mut self) -> Result<NodeId<PrimeExpr>> {
        let token = peek_expect!(
            self, "ident", "num", "string", "true", "false", "regex", "null", "this", "{", "[",
            "(", "function"
        );

        match token.kind() {
            t!("ident") => {
                self.next();
                let ident = token.kind_and_data.data_id().unwrap();
                let id = self.ast.push_node(PrimeExpr::Ident(ident));
                Ok(id)
            }
            t!("num") => {
                self.next();
                let id = self
                    .ast
                    .push_node(PrimeExpr::Number(token.kind_and_data.data_id().unwrap()));
                Ok(id)
            }
            t!("string") | t!("``") => {
                self.next();
                let id = self
                    .ast
                    .push_node(PrimeExpr::String(token.kind_and_data.data_id().unwrap()));
                Ok(id)
            }
            t!("true") => {
                self.next();
                let id = self.ast.push_node(PrimeExpr::Boolean(true));
                Ok(id)
            }
            t!("false") => {
                self.next();
                let id = self.ast.push_node(PrimeExpr::Boolean(false));
                Ok(id)
            }
            t!("regex") => {
                self.next();
                let id = self
                    .ast
                    .push_node(PrimeExpr::Regex(token.kind_and_data.data_id().unwrap()));
                Ok(id)
            }
            t!("null") => {
                self.next();
                let id = self.ast.push_node(PrimeExpr::Null);
                Ok(id)
            }
            t!("this") => {
                self.next();
                let id = self.ast.push_node(PrimeExpr::This);
                Ok(id)
            }
            t!("{") => {
                self.next();
                let id = self.parse_object_literal()?;
                Ok(self.ast.push_node(PrimeExpr::Object(id)))
            }
            t!("[") => {
                self.next();
                let array = self.parse_array_literal()?;
                Ok(self.ast.push_node(PrimeExpr::Array(array)))
            }
            t!("function") => {
                self.next();
                let func = self.parse_function(true)?;
                Ok(self.ast.push_node(PrimeExpr::Function(func)))
            }
            t!("(") => {
                self.next();
                let expression = self.parse_expr()?;
                expect!(self, ")");
                if self.ast[expression].next.is_some() {
                    peek_expect!(self, "=>");
                    self.reparse_arrow_function(expression)
                } else if let Some(t!("=>")) = self.peek_kind() {
                    self.reparse_arrow_function(expression)
                } else {
                    let expr = self.ast[expression].item;
                    let id = self.ast.push_node(PrimeExpr::Covered(expr));
                    Ok(id)
                }
            }
            TokenKind::UnreservedKeyword(_) => {
                let id = self.parse_ident()?;
                Ok(self.ast.push_node(PrimeExpr::Ident(id)))
            }
            x => {
                unexpected!(
                    self, x, "ident", "num", "string", "true", "false", "regex", "null", "this",
                    "{", "[", "(", "function"
                )
            }
        }
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
                    return Err(crate::Error::new(
                        crate::error::ErrorKind::UnexpectedEnd {
                            expected: vec![t!(":")],
                            message: None,
                        },
                        token.span,
                    ));
                }
            }
        }
    }

    fn parse_property_name(&mut self) -> Result<PropertyName> {
        let token = peek_expect!(self, "ident", "[", "string", "num");
        match token.kind() {
            t!("string") => {
                self.next();
                Ok(PropertyName::String(token.data_id().unwrap()))
            }
            t!("num") => {
                self.next();
                Ok(PropertyName::Number(token.data_id().unwrap()))
            }
            t!("[") => {
                self.next();
                let expr = self.parse_assignment_expr()?;
                expect!(self, "]");
                Ok(PropertyName::Computed(expr))
            }
            _ => {
                let ident = self.parse_ident_name()?;
                Ok(PropertyName::Ident(ident))
            }
        }
    }

    fn parse_array_literal(&mut self) -> Result<NodeId<ArrayLiteral>> {
        let mut prev = None;
        let mut head = None;
        let mut rest = None;

        loop {
            let token = peek_expect!(self, "]");
            match token.kind() {
                t!("]") => {
                    self.next();
                    break;
                }
                t!(",") => {
                    self.next();
                    prev = Some(self.ast.append_node_list(None, prev));
                    head = head.or(prev);
                }
                t!("...") => {
                    self.next();
                    let expr = self.parse_assignment_expr()?;
                    rest = Some(expr);
                }
                _ => {
                    let expr = self.parse_assignment_expr()?;
                    prev = Some(self.ast.append_node_list(Some(expr), prev));
                    head = head.or(prev);
                    if !self.eat(t!(",")) {
                        expect!(self, "]");
                        break;
                    }
                }
            }
        }

        Ok(self.ast.push_node(ArrayLiteral {
            elements: head,
            spread: rest,
        }))
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
