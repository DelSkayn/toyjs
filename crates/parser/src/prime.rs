use ast::{
    ArrayLiteralEntry, ArrowFunctionBody, AssignOp, BinaryOp, BindingElement, BindingPattern,
    BindingProperty, Expr, Function, FunctionKind, IdentOrPattern, ListHead, ListId, NodeId,
    ObjectLiteral, PrimeExpr, PropertyDefinition, PropertyName, Symbol, Template,
};
use common::{
    span::Span,
    string::{Ascii, String},
};
use token::{t, TokenKind};

use crate::{
    alter_state, expect, function::FunctionCtx, unexpected, Error, ErrorKind, Parser, ParserState,
    Result,
};

static YIELD_STR: &Ascii = Ascii::const_from_str("yield");
static AWAIT_STR: &Ascii = Ascii::const_from_str("await");

impl<'a> Parser<'a> {
    /// Parsers a primary expression or a expression without any operators.
    pub(crate) fn parse_prime(&mut self) -> Result<(NodeId<PrimeExpr>, Option<Span>)> {
        let token = self.peek();
        match token.kind() {
            t!("ident") => {
                let symbol = self.parse_symbol()?;
                if let t!("=>") = self.peek_kind() {
                    self.no_line_terminator()?;
                    self.next();
                    let param = self.ast.push_node(BindingElement::SingleName {
                        symbol,
                        initializer: None,
                    });
                    let params = ListHead::Present(self.ast.append_list(param, None));
                    let function = self.parse_arrow_function(params, None, FunctionKind::Simple)?;
                    let id = self.ast.push_node(PrimeExpr::Function(function));
                    Ok((id, None))
                } else {
                    let id = self.ast.push_node(PrimeExpr::Ident(symbol));
                    Ok((id, None))
                }
            }
            t!("123") => {
                self.next();
                let id = self
                    .ast
                    .push_node(PrimeExpr::Number(token.kind_and_data.data_id().unwrap()));
                Ok((id, None))
            }
            t!("string") | t!("``") => {
                self.next();
                let id = self
                    .ast
                    .push_node(PrimeExpr::String(token.kind_and_data.data_id().unwrap()));
                Ok((id, None))
            }
            t!("` ${") => {
                let template = self.parse_template()?;
                let id = self.ast.push_node(PrimeExpr::Template(template));
                Ok((id, None))
            }
            t!("true") => {
                self.next();
                let id = self.ast.push_node(PrimeExpr::Boolean(true));
                Ok((id, None))
            }
            t!("false") => {
                self.next();
                let id = self.ast.push_node(PrimeExpr::Boolean(false));
                Ok((id, None))
            }
            t!("/") | t!("/=") => {
                self.next();
                let token = self.lexer.relex_regex(token);
                if let token::TokenKind::Unknown = token.kind() {
                    return Err(Error::new(ErrorKind::InvalidToken, token.span));
                }
                let id = self
                    .ast
                    .push_node(PrimeExpr::Regex(token.kind_and_data.data_id().unwrap()));
                Ok((id, None))
            }
            t!("null") => {
                self.next();
                let id = self.ast.push_node(PrimeExpr::Null);
                Ok((id, None))
            }
            t!("this") => {
                self.next();
                let id = self.ast.push_node(PrimeExpr::This);
                Ok((id, None))
            }
            t!("super") => {
                self.next();
                let id = self.ast.push_node(PrimeExpr::Super);
                Ok((id, None))
            }
            t!("{") => {
                self.next();
                let (id, span) = self.parse_object_literal()?;
                Ok((self.ast.push_node(PrimeExpr::Object(id)), span))
            }
            t!("[") => {
                self.next();
                let array = self.parse_array_literal()?;
                Ok((self.ast.push_node(PrimeExpr::Array(array)), None))
            }
            t!("function") => {
                self.next();
                let kind = if self.eat(t!("*")) {
                    FunctionKind::Generator
                } else {
                    FunctionKind::Simple
                };
                let func = self.parse_function(FunctionCtx::Expression, kind)?;
                Ok((self.ast.push_node(PrimeExpr::Function(func)), None))
            }
            t!("async") => self.parse_async_function().map(|x| (x, None)),
            t!("class") => {
                self.next();
                let class = self.parse_class(false)?;
                Ok((self.ast.push_node(PrimeExpr::Class(class)), None))
            }
            t!("(") => {
                self.next();
                self.parse_covered_expression().map(|x| (x, None))
            }
            TokenKind::UnreservedKeyword(_) => {
                let symbol = self.parse_symbol()?;
                if let t!("=>") = self.peek_kind() {
                    self.no_line_terminator()?;
                    self.next();
                    let param = self.ast.push_node(BindingElement::SingleName {
                        symbol,
                        initializer: None,
                    });
                    let params = ListHead::Present(self.ast.append_list(param, None));
                    let function = self.parse_arrow_function(params, None, FunctionKind::Simple)?;
                    let id = self.ast.push_node(PrimeExpr::Function(function));
                    Ok((id, None))
                } else {
                    let id = self.ast.push_node(PrimeExpr::Ident(symbol));
                    Ok((id, None))
                }
            }
            x => {
                unexpected!(
                    self, x, "ident", "123", "string", "true", "false", "regex", "null", "this",
                    "{", "[", "(", "function"
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
    fn parse_covered_expression(&mut self) -> Result<NodeId<PrimeExpr>> {
        let mut head = ListHead::Empty;
        let mut prev = None;
        let mut rest = None;
        while t!(")") != self.peek_kind() {
            if self.eat(t!("...")) {
                rest = Some(self.parse_ident_or_pattern()?);
                break;
            }
            alter_state!(self => {
                self.state.insert(ParserState::In);
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
            self.peek();
            self.no_line_terminator()?;
            // TODO: Improve error message.
            expect!(self, "=>");
            return self.reparse_arrow_function(head, rest);
        }
        if let t!("=>") = self.peek_kind() {
            self.no_line_terminator()?;
            self.next();
            self.reparse_arrow_function(head, None)
        } else {
            let ListHead::Present(expr) = head else {
                unreachable!();
            };
            let id = self.ast.push_node(PrimeExpr::Covered(expr));
            Ok(id)
        }
    }

    /// Parses a template expression, e.g.:
    pub fn parse_template(&mut self) -> Result<NodeId<Template>> {
        let token = self.next();
        match token.kind() {
            t!("``") => {
                let text = token.data_id().unwrap();
                Ok(self.ast.push_node(Template::Tail { text }))
            }
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

    /// Parses a object literal expression, e.g.:
    /// ```javascript
    /// { /* start here */ foo: "bar" }
    /// ```
    ///
    /// Returns both the parsed object literal as well the span of the first '=' if the object
    /// literal contains a covered binding initalizer.
    fn parse_object_literal(&mut self) -> Result<(ObjectLiteral, Option<Span>)> {
        if let t!("}") = self.peek_kind() {
            self.next();
            return Ok((ObjectLiteral::Empty, None));
        }
        let (property, mut span) = self.parse_property_definition()?;
        let mut last = self.ast.append_list(property, None);
        let res = ObjectLiteral::Item(last);
        loop {
            match self.peek_kind() {
                t!("}") => {
                    self.next();
                    break;
                }
                t!(",") => {
                    self.next();
                    if let t!("}") = self.peek_kind() {
                        self.next();
                        break;
                    }
                    let (property, new_span) = self.parse_property_definition()?;
                    span = span.or(new_span);
                    last = self.ast.append_list(property, Some(last));
                }
                x => {
                    unexpected!(self, x, ",", "}")
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
    fn parse_property_definition(&mut self) -> Result<(NodeId<PropertyDefinition>, Option<Span>)> {
        let token = self.peek();
        let property = match token.kind() {
            t!("...") => {
                self.next();
                let expr = self.parse_assignment_expr()?;
                let id = self.ast.push_node(PropertyDefinition::Rest(expr));
                return Ok((id, None));
            }
            t!("*") => {
                self.next();
                let property = self.parse_property_name()?;
                let func = self.parse_function(FunctionCtx::Method, FunctionKind::Generator)?;
                return Ok((
                    self.ast
                        .push_node(PropertyDefinition::Method { property, func }),
                    None,
                ));
            }
            t!("get") => {
                self.next();
                if Self::is_property_name(self.peek_kind()) {
                    let property = self.parse_property_name()?;
                    let func = self.parse_getter()?;
                    return Ok((
                        self.ast
                            .push_node(PropertyDefinition::Getter { property, func }),
                        None,
                    ));
                }
                PropertyName::Ident(self.lexer.data.strings.intern(&String::new_const("get")))
            }
            t!("set") => {
                self.next();
                if Self::is_property_name(self.peek_kind()) {
                    let property = self.parse_property_name()?;
                    let func = self.parse_setter()?;
                    return Ok((
                        self.ast
                            .push_node(PropertyDefinition::Setter { property, func }),
                        None,
                    ));
                }
                PropertyName::Ident(self.lexer.data.strings.intern(&String::new_const("set")))
            }
            t!("async") => {
                self.next();
                if let t!(":") = self.peek_kind() {
                    PropertyName::Ident(self.lexer.data.strings.intern(&String::new_const("async")))
                } else {
                    self.no_line_terminator()?;
                    let kind = if self.eat(t!("*")) {
                        FunctionKind::AsyncGenerator
                    } else {
                        FunctionKind::Generator
                    };
                    let property = self.parse_property_name()?;
                    let func = self.parse_function(FunctionCtx::Method, kind)?;
                    return Ok((
                        self.ast
                            .push_node(PropertyDefinition::Method { property, func }),
                        None,
                    ));
                }
            }
            _ => self.parse_property_name()?,
        };

        match self.peek_kind() {
            t!(":") => {
                self.next();
                let expr = self.parse_assignment_expr()?;
                let id = self
                    .ast
                    .push_node(PropertyDefinition::Define { property, expr });
                Ok((id, None))
            }
            t!("=") => {
                if let PropertyName::Ident(x) = property {
                    let span = self.next().span;
                    let expr = self.parse_assignment_expr()?;
                    let symbol = self.ast.push_node(Symbol {
                        name: x,
                        span: token.span,
                    });

                    let id = self.ast.push_node(PropertyDefinition::Covered {
                        symbol,
                        initializer: expr,
                    });
                    Ok((id, Some(span)))
                } else {
                    unexpected!(self, t!("="), ":")
                }
            }
            t!("(") => {
                let func = self.parse_function(FunctionCtx::Method, FunctionKind::Simple)?;
                Ok((
                    self.ast
                        .push_node(PropertyDefinition::Method { property, func }),
                    None,
                ))
            }
            x => {
                if let PropertyName::Ident(name) = property {
                    let span = self.ast.push_node(token.span);
                    let id = self.ast.push_node(PropertyDefinition::Ident { name, span });
                    Ok((id, None))
                } else {
                    unexpected!(self, x, ":")
                }
            }
        }
    }

    pub fn parse_property_name(&mut self) -> Result<PropertyName> {
        let token = self.peek();
        match token.kind() {
            t!("string") => {
                self.next();
                Ok(PropertyName::String(token.data_id().unwrap()))
            }
            t!("123") => {
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

    fn parse_array_literal(&mut self) -> Result<ListHead<ArrayLiteralEntry>> {
        let mut prev = None;
        let mut head = ListHead::Empty;

        loop {
            match self.peek_kind() {
                t!("]") => {
                    break;
                }
                t!(",") => {
                    self.next();
                    let node = self.ast.push_node(ArrayLiteralEntry {
                        expr: None,
                        is_spread: false,
                    });
                    prev = Some(self.ast.append_list(node, prev));
                    head = head.or(prev.into());
                }
                t!("...") => {
                    self.next();
                    let expr = self.parse_assignment_expr()?;
                    let node = self.ast.push_node(ArrayLiteralEntry {
                        expr: Some(expr),
                        is_spread: true,
                    });
                    prev = Some(self.ast.append_list(node, prev));
                    head = head.or(prev.into());
                    if !self.eat(t!(",")) {
                        break;
                    }
                }
                _ => {
                    let expr = self.parse_assignment_expr()?;
                    let node = self.ast.push_node(ArrayLiteralEntry {
                        expr: Some(expr),
                        is_spread: false,
                    });
                    prev = Some(self.ast.append_list(node, prev));
                    head = head.or(prev.into());
                    if !self.eat(t!(",")) {
                        break;
                    }
                }
            }
        }
        expect!(self, "]");
        Ok(head)
    }

    pub fn reparse_arrow_function(
        &mut self,
        expr: ListHead<Expr>,
        rest: Option<NodeId<IdentOrPattern>>,
    ) -> Result<NodeId<PrimeExpr>> {
        let mut head = ListHead::Empty;
        let mut prev = None;

        let mut cur: Option<ListId<Expr>> = expr.into();
        while let Some(expr) = cur {
            let Some(param) = self.reparse_binding_element(self.ast[expr].item) else {
                unexpected!(self,t!("=>") => "covered expression can't be parsed as parameters");
            };
            let param = self.ast.push_node(param);
            prev = Some(self.ast.append_list(param, prev));
            head = head.or(prev.into());
            cur = self.ast[expr].next;
        }
        let func = self.parse_arrow_function(head, rest, FunctionKind::Simple)?;
        Ok(self.ast.push_node(PrimeExpr::Function(func)))
    }

    fn reparse_binding_element(&mut self, expr: NodeId<Expr>) -> Option<BindingElement> {
        match self.ast[expr] {
            Expr::Prime { expr } => match self.ast[expr] {
                PrimeExpr::Ident(name) => Some(BindingElement::SingleName {
                    symbol: name,
                    initializer: None,
                }),
                PrimeExpr::Object(x) => {
                    let pattern = self.reparse_object_lit(x)?;
                    let pattern = self.ast.push_node(pattern);
                    Some(BindingElement::Pattern {
                        pattern,
                        initializer: None,
                    })
                }
                PrimeExpr::Array(x) => {
                    let pattern = self.reparse_array_lit(x)?;
                    let pattern = self.ast.push_node(pattern);
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

    pub fn reparse_object_lit(&mut self, expr: ObjectLiteral) -> Option<BindingPattern> {
        let ObjectLiteral::Item(mut item) = expr else {
            return Some(BindingPattern::Object {
                properties: ListHead::Empty,
                rest: None,
            });
        };

        let mut properties = ListHead::Empty;
        let mut prev = None;
        let mut rest = None;
        loop {
            let item_node = self.ast[item].item;
            let prop = match self.ast[item_node] {
                PropertyDefinition::Ident { name, span } => {
                    let span = self.ast[span];
                    BindingProperty::Binding {
                        symbol: self.ast.push_node(Symbol { name, span }),
                        initializer: None,
                    }
                }

                // TODO: make sure this is a syntax error in actual object literals
                PropertyDefinition::Covered {
                    symbol: ident,
                    initializer,
                } => BindingProperty::Binding {
                    symbol: ident,
                    initializer: Some(initializer),
                },
                PropertyDefinition::Define { property, expr } => {
                    let element = self.reparse_binding_element(expr)?;
                    let element = self.ast.push_node(element);
                    BindingProperty::Property {
                        name: property,
                        element,
                    }
                }
                PropertyDefinition::Method { .. }
                | PropertyDefinition::Getter { .. }
                | PropertyDefinition::Setter { .. } => return None,
                PropertyDefinition::Rest(r) => {
                    // Rest not last in the object
                    if rest.is_some() || self.ast[item].next.is_some() {
                        return None;
                    }
                    let Expr::Prime { expr } = self.ast[r] else {
                        return None;
                    };
                    let PrimeExpr::Ident(x) = self.ast[expr] else {
                        return None;
                    };
                    rest = Some(x);
                    // rest is last
                    break;
                }
            };
            let prop = self.ast.push_node(prop);
            prev = Some(self.ast.append_list(prop, prev));
            properties = properties.or(prev.into());

            let Some(next) = self.ast[item].next else {
                break;
            };
            item = next;
        }

        Some(BindingPattern::Object { properties, rest })
    }

    pub fn reparse_array_lit(
        &mut self,
        expr: ListHead<ArrayLiteralEntry>,
    ) -> Option<BindingPattern> {
        let ListHead::Present(head) = expr else {
            return Some(BindingPattern::Array {
                elements: None,
                rest: None,
            });
        };

        let mut cur = Some(head);
        let mut head = None;
        let mut prev = None;
        let mut rest = None;

        while let Some(c) = cur {
            let item = self.ast[c].item;
            let element = if let Some(x) = self.ast[item].expr {
                if self.ast[item].is_spread {
                    if self.ast[c].next.is_some() {
                        return None;
                    } else {
                        let elem = self.reparse_ident_or_pattern(x)?;
                        rest = Some(self.ast.push_node(elem));
                        break;
                    }
                } else {
                    let elem = self.reparse_binding_element(x)?;
                    Some(self.ast.push_node(elem))
                }
            } else {
                None
            };
            prev = Some(self.ast.append_node_list(element, prev));
            head = head.or(prev);
            cur = self.ast[c].next;
        }

        Some(BindingPattern::Array {
            elements: head,
            rest,
        })
    }

    fn reparse_ident_or_pattern(&mut self, expr: NodeId<Expr>) -> Option<IdentOrPattern> {
        let Expr::Prime { expr } = self.ast[expr] else {
            return None;
        };
        match self.ast[expr] {
            PrimeExpr::Ident(name) => Some(IdentOrPattern::Ident(name)),
            PrimeExpr::Object(lit) => {
                let pat = self.reparse_object_lit(lit)?;
                let pat = self.ast.push_node(pat);
                Some(IdentOrPattern::Pattern(pat))
            }
            PrimeExpr::Array(lit) => {
                let pat = self.reparse_array_lit(lit)?;
                let pat = self.ast.push_node(pat);
                Some(IdentOrPattern::Pattern(pat))
            }
            _ => None,
        }
    }

    fn parse_arrow_function(
        &mut self,
        params: ListHead<BindingElement>,
        rest_param: Option<NodeId<IdentOrPattern>>,
        kind: FunctionKind,
    ) -> Result<NodeId<Function>> {
        let state = self.state;

        self.state.set(
            ParserState::AwaitIdent,
            matches!(kind, FunctionKind::Simple | FunctionKind::Generator),
        );
        let body = if let t!("{") = self.peek_kind() {
            self.next();
            let mut head = ListHead::Empty;
            let mut prev = None;

            while !self.eat(t!("}")) {
                let stmt = self.parse_stmt()?;
                if prev.is_none()
                    && !self.state.contains(ParserState::Strict)
                    && self.is_strict_directive(stmt)
                {
                    self.state.insert(ParserState::Strict);
                }
                prev = Some(self.ast.append_list(stmt, prev));
                head = head.or(prev.into());
            }
            ArrowFunctionBody::Stmt(head)
        } else {
            ArrowFunctionBody::Expr(self.parse_assignment_expr()?)
        };

        let is_strict = state.contains(ParserState::Strict);
        self.state = state;
        Ok(self.ast.push_node(Function::Arrow {
            is_strict,
            kind,
            params,
            rest_param,
            body,
        }))
    }

    fn parse_async_function(&mut self) -> Result<NodeId<PrimeExpr>> {
        self.next();
        let token = self.peek_kind();
        self.no_line_terminator()?;
        // TODO not sure if this the right way to do it.
        match token {
            t!("function") => {
                self.next();
                let kind = if self.eat(t!("*")) {
                    FunctionKind::AsyncGenerator
                } else {
                    FunctionKind::Async
                };
                let func = self.parse_function(FunctionCtx::Expression, kind)?;
                Ok(self.ast.push_node(PrimeExpr::Function(func)))
            }
            t!("(") => {
                self.next();
                let mut head = ListHead::Empty;
                let mut prev = None;
                let mut rest = None;
                while !self.eat(t!(")")) {
                    if self.eat(t!("...")) {
                        rest = Some(self.parse_ident_or_pattern()?);
                    } else {
                        let element = self.parse_binding_element()?;
                        let element = self.ast.push_node(element);
                        prev = Some(self.ast.append_list(element, prev));
                        head = head.or(prev.into());
                    }
                }
                self.peek();
                // TODO: Improve error her if no => is found
                self.no_line_terminator()?;
                expect!(self, "=>");
                let func = self.parse_arrow_function(head, rest, FunctionKind::Async)?;
                Ok(self.ast.push_node(PrimeExpr::Function(func)))
            }
            _ => {
                let symbol = self.parse_symbol()?;
                let element = self.ast.push_node(BindingElement::SingleName {
                    symbol,
                    initializer: None,
                });
                let element = self.ast.append_list(element, None);
                let element = ListHead::Present(element);
                self.peek();
                // TODO: Improve error her if no => is found
                self.no_line_terminator()?;
                expect!(self, "=>");
                let func = self.parse_arrow_function(element, None, FunctionKind::Async)?;
                Ok(self.ast.push_node(PrimeExpr::Function(func)))
            }
        }
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
            Ok((x, _)) => {
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
            Ok((x, _)) => {
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
            Ok((x, _)) => {
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
            Ok((x, _)) => {
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
            Ok((x, _)) => {
                let PrimeExpr::Object(ObjectLiteral::Item(node)) = parser.ast[x] else {
                    panic!("not object");
                };
                let item = parser.ast[node].item;
                assert!(matches!(parser.ast[item], PropertyDefinition::Ident { .. }));
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
            Ok((x, _)) => {
                assert!(matches!(parser.ast[x], ast::PrimeExpr::Array(_)))
            }
            Err(_) => {
                panic!()
            }
        }
    }

    #[test]
    fn object_literal_covered() {
        create_test_parser!("{ a = 3 }", parser);
        match parser.parse_prime() {
            Ok((x, span)) => {
                assert!(span.is_some());
                assert!(matches!(parser.ast[x], ast::PrimeExpr::Object(_)))
            }
            Err(_) => {
                panic!()
            }
        }
    }
}
