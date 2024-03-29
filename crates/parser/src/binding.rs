use ast::{
    BindingElement, BindingPattern, BindingProperty, IdentOrPattern, ListHead, NodeId,
    PropertyName, Symbol,
};
use common::string::{String, StringId};
use token::{t, TokenKind};

use crate::{expect, unexpected, Parser, ParserState, Result};

impl<'a> Parser<'a> {
    pub fn parse_ident_or_pattern(&mut self) -> Result<NodeId<IdentOrPattern>> {
        match self.peek_kind() {
            t!("{") | t!("[") => {
                let pattern = self.parse_pattern()?;
                Ok(self.ast.push_node(IdentOrPattern::Pattern(pattern)))
            }
            _ => {
                let ident = self.parse_symbol()?;
                Ok(self.ast.push_node(IdentOrPattern::Ident(ident)))
            }
        }
    }

    pub fn parse_pattern(&mut self) -> Result<NodeId<BindingPattern>> {
        match self.next().kind() {
            t!("{") => self.parse_object_pattern(),
            t!("[") => self.parse_array_pattern(),
            x => unexpected!(self, x, "{", "["),
        }
    }

    pub fn parse_ident_name(&mut self) -> Result<StringId> {
        let next = self.next();
        let text = match next.kind() {
            t!("ident") => return Ok(next.data_id().unwrap()),
            TokenKind::Keyword(x) => x.to_string(),
            TokenKind::UnreservedKeyword(x) => x.to_string(),
            x => unexpected!(self, x, "ident"),
        };

        Ok(self.lexer.data.strings.intern(&text))
    }

    pub fn parse_symbol(&mut self) -> Result<NodeId<Symbol>> {
        let name = self.parse_ident()?;
        let span = *self.last_span();
        let res = self.ast.push_node(Symbol { name, span });
        Ok(res)
    }

    pub fn parse_ident(&mut self) -> Result<StringId> {
        let next = self.next();
        match next.kind() {
            t!("ident") => Ok(next.data_id().unwrap()),
            t!("yield") => {
                if self.state.contains(ParserState::YieldIdent) {
                    Ok(self.lexer.data.strings.intern(&String::new_const("yield")))
                } else {
                    unexpected!(self,t!("yield"),"ident" => "not allowed as an identifier in this context");
                }
            }
            t!("await") => {
                if self.state.contains(ParserState::AwaitIdent) {
                    Ok(self.lexer.data.strings.intern(&String::new_const("await")))
                } else {
                    unexpected!(self,t!("await"),"ident" => "not allowed as an identifier in this context");
                }
            }
            t!("let") => {
                if self.state.contains(ParserState::Strict) {
                    unexpected!(self,t!("let"),"ident" => "not allowed as an identifier in strict mode");
                } else {
                    Ok(self.lexer.data.strings.intern(&String::new_const("let")))
                }
            }
            t!("static") => {
                if self.state.contains(ParserState::Strict) {
                    unexpected!(self,t!("static"),"ident" => "not allowed as an identifier in strict mode");
                } else {
                    Ok(self.lexer.data.strings.intern(&String::new_const("static")))
                }
            }
            t!("implements") => {
                if self.state.contains(ParserState::Strict) {
                    unexpected!(self,t!("implements"),"ident" => "not allowed as an identifier in strict mode");
                } else {
                    Ok(self
                        .lexer
                        .data
                        .strings
                        .intern(&String::new_const("implements")))
                }
            }
            t!("interface") => {
                if self.state.contains(ParserState::Strict) {
                    unexpected!(self,t!("interface"),"ident" => "not allowed as an identifier in strict mode");
                } else {
                    Ok(self
                        .lexer
                        .data
                        .strings
                        .intern(&String::new_const("interface")))
                }
            }
            t!("package") => {
                if self.state.contains(ParserState::Strict) {
                    unexpected!(self,t!("package"),"ident" => "not allowed as an identifier in strict mode");
                } else {
                    Ok(self
                        .lexer
                        .data
                        .strings
                        .intern(&String::new_const("package")))
                }
            }
            t!("private") => {
                if self.state.contains(ParserState::Strict) {
                    unexpected!(self,t!("private"),"ident" => "not allowed as an identifier in strict mode");
                } else {
                    Ok(self
                        .lexer
                        .data
                        .strings
                        .intern(&String::new_const("private")))
                }
            }
            t!("protected") => {
                if self.state.contains(ParserState::Strict) {
                    unexpected!(self,t!("protected"),"ident" => "not allowed as an identifier in strict mode");
                } else {
                    Ok(self
                        .lexer
                        .data
                        .strings
                        .intern(&String::new_const("protected")))
                }
            }
            t!("public") => {
                if self.state.contains(ParserState::Strict) {
                    unexpected!(self,t!("public"),"ident" => "not allowed as an identifier in strict mode");
                } else {
                    Ok(self.lexer.data.strings.intern(&String::new_const("public")))
                }
            }
            // Next keywords are always allowed as identifiers.
            TokenKind::UnreservedKeyword(x) => Ok(self.lexer.data.strings.intern(&x.to_string())),
            x => unexpected!(self, x, "ident"),
        }
    }

    pub fn parse_object_pattern(&mut self) -> Result<NodeId<BindingPattern>> {
        let mut head = ListHead::Empty;
        let mut prev = None;
        let mut rest = None;
        loop {
            match self.peek_kind() {
                t!("}") => {
                    self.next();
                    break;
                }
                t!("...") => {
                    self.next();
                    let symbol = self.parse_symbol()?;

                    expect!(self, "}");
                    rest = Some(symbol);
                    break;
                }
                _ => {
                    let prop = self.parse_binding_property()?;
                    let item = self.ast.push_node(prop);
                    prev = Some(self.ast.append_list(item, prev));
                    head = head.or(prev.into());
                }
            }
            if !self.eat(t!(",")) {
                expect!(self, "}");
                break;
            }
        }

        Ok(self.ast.push_node(BindingPattern::Object {
            properties: head,
            rest,
        }))
    }

    pub fn parse_binding_property(&mut self) -> Result<BindingProperty> {
        let next = self.peek();
        match next.kind() {
            t!("string") => {
                self.next();
                expect!(self, ":");
                let name = PropertyName::String(next.data_id().unwrap());
                let element = self.parse_binding_element()?;
                let element = self.ast.push_node(element);
                Ok(BindingProperty::Property { name, element })
            }
            t!("123") => {
                self.next();
                expect!(self, ":");
                let name = PropertyName::Number(next.data_id().unwrap());
                let element = self.parse_binding_element()?;
                let element = self.ast.push_node(element);
                Ok(BindingProperty::Property { name, element })
            }
            t!("[") => {
                self.next();
                let expr = self.parse_assignment_expr()?;
                expect!(self, "]");
                expect!(self, ":");
                let name = PropertyName::Computed(expr);
                let element = self.parse_binding_element()?;
                let element = self.ast.push_node(element);
                Ok(BindingProperty::Property { name, element })
            }
            _ => {
                let ident = self.parse_ident_name()?;
                let span = *self.last_span();
                if self.eat(t!(":")) {
                    let name = PropertyName::Ident(ident);
                    let element = self.parse_binding_element()?;
                    let element = self.ast.push_node(element);
                    Ok(BindingProperty::Property { name, element })
                } else {
                    let initializer = self
                        .eat(t!("="))
                        .then(|| self.parse_assignment_expr())
                        .transpose()?;

                    let symbol = self.ast.push_node(Symbol { name: ident, span });

                    Ok(BindingProperty::Binding {
                        symbol,
                        initializer,
                    })
                }
            }
        }
    }

    pub fn parse_array_pattern(&mut self) -> Result<NodeId<BindingPattern>> {
        let mut head = None;
        let mut prev = None;
        let mut rest = None;
        loop {
            match self.peek_kind() {
                t!("]") => {
                    break;
                }
                t!("...") => {
                    self.next();
                    rest = Some(self.parse_ident_or_pattern()?);
                    break;
                }
                t!(",") => {
                    self.next();
                    prev = Some(self.ast.append_node_list(None, prev));
                    head = head.or(prev);
                }
                _ => {
                    let elem = self.parse_binding_element()?;
                    let item = self.ast.push_node(elem);
                    prev = Some(self.ast.append_node_list(Some(item), prev));
                    head = head.or(prev);
                    if !self.eat(t!(",")) {
                        break;
                    }
                }
            }
        }
        expect!(self, "]");

        Ok(self.ast.push_node(BindingPattern::Array {
            elements: head,
            rest,
        }))
    }

    pub fn parse_binding_element(&mut self) -> Result<BindingElement> {
        match self.peek_kind() {
            t!("{") | t!("[") => {
                let pattern = self.parse_pattern()?;
                let initializer = self
                    .eat(t!("="))
                    .then(|| self.parse_assignment_expr())
                    .transpose()?;
                Ok(BindingElement::Pattern {
                    pattern,
                    initializer,
                })
            }
            _ => {
                let symbol = self.parse_symbol()?;
                let initializer = self
                    .eat(t!("="))
                    .then(|| self.parse_assignment_expr())
                    .transpose()?;
                Ok(BindingElement::SingleName {
                    symbol,
                    initializer,
                })
            }
        }
    }
}
