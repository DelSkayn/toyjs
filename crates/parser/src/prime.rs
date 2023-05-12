use std::mem;

use ast::{
    ArrayLiteral, ArrowFunctionBody, AssignOp, BinaryOp, BindingElement, BindingPattern,
    BindingProperty, Expr, Function, IdentOrPattern, ListHead, ListId, NodeId, ObjectLiteral,
    PrimeExpr, PropertyDefinition, PropertyName, Template,
};
use common::string::Ascii;
use token::{t, TokenKind};

use crate::{alter_state, expect, next_expect, peek_expect, unexpected, Parser, Result};

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
                if let Some(t!("=>")) = self.peek_kind() {
                    let param = self.ast.push_node(BindingElement::SingleName {
                        name: ident,
                        initializer: None,
                    });

                    let params = ListHead::Present(self.ast.append_list(param, None));
                    let function = self.parse_array_function(params, None)?;
                    Ok(self.ast.push_node(PrimeExpr::Function(function)))
                } else {
                    let id = self.ast.push_node(PrimeExpr::Ident(ident));
                    Ok(id)
                }
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
            t!("` ${") => {
                let template = self.parse_template()?;
                let id = self.ast.push_node(PrimeExpr::Template(template));
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
            t!("/") | t!("/=") => {
                self.next();
                let token = self.lexer.relex_regex(token);
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
                self.parse_covered_expression()
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

    fn parse_covered_expression(&mut self) -> Result<NodeId<PrimeExpr>> {
        let mut head = ListHead::Empty;
        let mut prev = None;
        let mut rest = None;
        while t!(")") != peek_expect!(self, ")").kind() {
            if self.eat(t!("...")) {
                rest = Some(self.parse_assignment_expr()?);
                break;
            }
            alter_state!(self,r#in = true => {
                let expr = self.parse_assignment_expr()?;
            });
            prev = Some(self.ast.append_list(expr, prev));
            head = head.or(prev.into());
            if !self.eat(t!(",")) {
                break;
            }
        }
        expect!(self, ")");

        // spread operator or an empty head always indicate an arrow function.
        if rest.is_some() || head.is_empty() {
            return self.reparse_arrow_function(head, rest);
        }
        if let Some(t!("=>")) = self.peek_kind() {
            self.reparse_arrow_function(head, None)
        } else {
            let ListHead::Present(expr) = head else {
                unreachable!();
            };
            let id = self.ast.push_node(PrimeExpr::Covered(expr));
            Ok(id)
        }
    }

    fn parse_template(&mut self) -> Result<NodeId<Template>> {
        let token = next_expect!(self, "} `");
        match token.kind() {
            t!("` ${") => {
                let expr = self.parse_expr()?;
                let text = token.data_id().unwrap();
                let next = self.parse_template()?;
                Ok(self.ast.push_node(Template::Head { text, expr, next }))
            }
            t!("}") => {
                let token = self.lexer.relex_template(token);
                match token.kind() {
                    t!("} ${") => {
                        let expr = self.parse_expr()?;
                        let text = token.data_id().unwrap();
                        let next = self.parse_template()?;
                        Ok(self.ast.push_node(Template::Head { text, expr, next }))
                    }
                    t!("} `") => {
                        let text = token.data_id().unwrap();
                        Ok(self.ast.push_node(Template::Tail { text }))
                    }
                    x => unexpected!(self, x, "} `"),
                }
            }
            x => unexpected!(self, x),
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

    fn reparse_arrow_function(
        &mut self,
        expr: ListHead<Expr>,
        rest: Option<NodeId<Expr>>,
    ) -> Result<NodeId<PrimeExpr>> {
        let mut head = ListHead::Empty;
        let mut prev = None;

        let mut cur: Option<ListId<Expr>> = expr.into();
        while let Some(expr) = cur {
            let Some(param) = self.reparse_binding_element(self.ast[expr].item) else{
                unexpected!(self,t!("=>") => "covered expression can't be parsed as parameters");
            };
            let param = self.ast.push_node(param);
            prev = Some(self.ast.append_list(param, prev));
            head = head.or(prev.into());
            cur = self.ast[expr].next;
        }
        let rest = if let Some(rest) = rest {
            let Some(rest) = self.reparse_ident_or_pattern(rest) else {
                unexpected!(self,t!("=>") => "covered expression can't be parsed as parameters");
            };
            Some(self.ast.push_node(rest))
        } else {
            None
        };
        let func = self.parse_array_function(head, rest)?;
        Ok(self.ast.push_node(PrimeExpr::Function(func)))
    }

    fn reparse_binding_element(&mut self, expr: NodeId<Expr>) -> Option<BindingElement> {
        match self.ast[expr] {
            Expr::Prime { expr } => match self.ast[expr] {
                PrimeExpr::Ident(name) => Some(BindingElement::SingleName {
                    name,
                    initializer: None,
                }),
                PrimeExpr::Object(x) => {
                    let pattern = self.reparse_object_binding(x)?;
                    Some(BindingElement::Pattern {
                        pattern,
                        initializer: None,
                    })
                }
                PrimeExpr::Array(x) => {
                    let pattern = self.reparse_array_lit(x)?;
                    Some(BindingElement::Pattern {
                        pattern,
                        initializer: None,
                    })
                }
                _ => None,
            },
            Expr::Binary {
                op: BinaryOp::Assign(AssignOp::Assign),
                left,
                right,
            } => {
                let mut binding = self.reparse_binding_element(left)?;
                let (BindingElement::SingleName {
                    ref mut initializer,
                    ..
                }
                | BindingElement::Pattern {
                    ref mut initializer,
                    ..
                }) = binding;
                debug_assert!(initializer.is_none());
                *initializer = Some(right);
                Some(binding)
            }
            _ => None,
        }
    }

    fn reparse_object_binding(&mut self, expr: ObjectLiteral) -> Option<BindingPattern> {
        let ObjectLiteral::Item(mut item) = expr else {
            return Some(BindingPattern::Object {
                properties: ListHead::Empty,
                rest: None,
            })
        };

        let mut properties = ListHead::Empty;
        let mut prev = None;
        let mut rest = None;
        loop {
            let item_node = self.ast[item].item;
            let prop = match self.ast[item_node] {
                PropertyDefinition::Ident(name) => BindingProperty::Binding {
                    name,
                    initializer: None,
                },
                // TODO: make sure this is a syntax error in actual object literals
                PropertyDefinition::Covered { ident, initializer } => BindingProperty::Binding {
                    name: ident,
                    initializer: Some(initializer),
                },
                PropertyDefinition::Define { property, expr } => {
                    let element = self.reparse_binding_element(expr)?;
                    BindingProperty::Property {
                        name: property,
                        element,
                    }
                }
                PropertyDefinition::Method {} => return None,
                PropertyDefinition::Rest(r) => {
                    // Rest not last in the object
                    if rest.is_some() || self.ast[item].next.is_some() {
                        return None;
                    }
                    let Expr::Prime { expr } = self.ast[r] else {
                        return None
                    };
                    let PrimeExpr::Ident(x) = self.ast[expr] else {
                        return None
                    };
                    rest = Some(x);
                    // rest is last
                    break;
                }
            };
            let prop = self.ast.push_node(prop);
            prev = Some(self.ast.append_list(prop, prev));
            properties = properties.or(prev.into());

            let Some(next) = self.ast[item].next else{
                break;
            };
            item = next;
        }

        Some(BindingPattern::Object { properties, rest })
    }

    fn reparse_array_lit(&mut self, expr: NodeId<ArrayLiteral>) -> Option<BindingPattern> {
        let rest = if let Some(x) = self.ast[expr].spread {
            let rest = self.reparse_ident_or_pattern(x)?;
            Some(self.ast.push_node(rest))
        } else {
            None
        };

        let mut cur = self.ast[expr].elements;
        let mut head = ListHead::Empty;
        let mut prev = None;

        while let Some(c) = cur {
            let element = if let Some(x) = self.ast[c].data {
                Some(self.reparse_binding_element(x)?)
            } else {
                None
            };
            let element = self.ast.push_node(element);
            prev = Some(self.ast.append_list(element, prev));
            head = head.or(prev.into());
            cur = self.ast[c].next;
        }

        Some(BindingPattern::Array {
            elements: head,
            rest,
        })
    }

    fn reparse_ident_or_pattern(&mut self, expr: NodeId<Expr>) -> Option<IdentOrPattern> {
        let Expr::Prime { expr } = self.ast[expr] else {
            return None
        };
        match self.ast[expr] {
            PrimeExpr::Ident(name) => Some(IdentOrPattern::Ident(name)),
            PrimeExpr::Object(lit) => {
                let pat = self.reparse_object_binding(lit)?;
                Some(IdentOrPattern::Pattern(pat))
            }
            PrimeExpr::Array(lit) => {
                let pat = self.reparse_array_lit(lit)?;
                Some(IdentOrPattern::Pattern(pat))
            }
            _ => None,
        }
    }

    fn parse_array_function(
        &mut self,
        params: ListHead<BindingElement>,
        rest_param: Option<NodeId<IdentOrPattern>>,
    ) -> Result<NodeId<Function>> {
        expect!(self, "=>");
        let mut strict = self.state.strict;
        let body = if let t!("{") = peek_expect!(self, "{").kind() {
            self.next();
            let mut head = ListHead::Empty;
            let mut prev = None;

            while !self.eat(t!("}")) {
                let stmt = self.parse_stmt()?;
                if prev.is_none() && !strict {
                    self.state.strict = self.is_strict_directive(stmt);
                }
                prev = Some(self.ast.append_list(stmt, prev));
                head = head.or(prev.into());
            }
            mem::swap(&mut self.state.strict, &mut strict);
            ArrowFunctionBody::Stmt(head)
        } else {
            ArrowFunctionBody::Expr(self.parse_assignment_expr()?)
        };

        Ok(self.ast.push_node(Function::Arrow {
            strict,
            params,
            rest_param,
            body,
        }))
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
