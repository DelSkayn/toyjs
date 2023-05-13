use ast::{
    BindingElement, BindingPattern, BindingProperty, IdentOrPattern, ListHead, NodeId, PropertyName,
};
use common::string::String;
use token::{t, StringId, TokenKind};

use crate::{expect, next_expect, peek_expect, unexpected, Parser, Result};

impl<'a> Parser<'a> {
    pub fn parse_ident_or_pattern(&mut self) -> Result<NodeId<IdentOrPattern>> {
        let first = peek_expect!(self, "ident", "{", "[");
        match first.kind() {
            t!("{") | t!("[") => {
                let pattern = self.parse_pattern()?;
                Ok(self.ast.push_node(IdentOrPattern::Pattern(pattern)))
            }
            _ => {
                let ident = self.parse_ident()?;
                Ok(self.ast.push_node(IdentOrPattern::Ident(ident)))
            }
        }
    }

    pub fn parse_pattern(&mut self) -> Result<BindingPattern> {
        let first = next_expect!(self, "{", "[");
        match first.kind() {
            t!("{") => self.parse_object_pattern(),
            t!("[") => self.parse_array_pattern(),
            x => unexpected!(self, x, "{", "["),
        }
    }

    pub fn parse_ident_name(&mut self) -> Result<StringId> {
        let next = next_expect!(self);
        let text = match next.kind() {
            t!("ident") => return Ok(next.data_id().unwrap()),
            TokenKind::Keyword(x) => x.to_string(),
            TokenKind::UnreservedKeyword(x) => x.to_string(),
            x => unexpected!(self, x, "ident"),
        };

        Ok(self.lexer.data.strings.intern(&text))
    }

    pub fn parse_ident(&mut self) -> Result<StringId> {
        let next = next_expect!(self);
        match next.kind() {
            t!("ident") => Ok(next.data_id().unwrap()),
            t!("yield") => {
                if self.state.yield_ident {
                    unexpected!(self,t!("yield"),"ident" => "not allowed as an identifier in this context");
                } else {
                    Ok(self.lexer.data.strings.intern(&String::new_const("yield")))
                }
            }
            t!("await") => {
                if self.state.await_ident {
                    unexpected!(self,t!("await"),"ident" => "not allowed as an identifier in this context");
                } else {
                    Ok(self.lexer.data.strings.intern(&String::new_const("await")))
                }
            }
            t!("let") => {
                if self.state.strict {
                    unexpected!(self,t!("let"),"ident" => "not allowed as an identifier in strict mode");
                } else {
                    Ok(self.lexer.data.strings.intern(&String::new_const("let")))
                }
            }
            t!("static") => {
                if self.state.strict {
                    unexpected!(self,t!("static"),"ident" => "not allowed as an identifier in strict mode");
                } else {
                    Ok(self.lexer.data.strings.intern(&String::new_const("static")))
                }
            }
            t!("implements") => {
                if self.state.strict {
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
                if self.state.strict {
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
                if self.state.strict {
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
                if self.state.strict {
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
                if self.state.strict {
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
                if self.state.strict {
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

    pub fn parse_object_pattern(&mut self) -> Result<BindingPattern> {
        let mut head = ListHead::Empty;
        let mut prev = None;
        let mut rest = None;
        loop {
            let next = peek_expect!(self, "}");
            match next.kind() {
                t!("}") => {
                    self.next();
                    break;
                }
                t!("...") => {
                    self.next();
                    let ident = self.parse_ident()?;
                    expect!(self, "}");
                    rest = Some(ident);
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

        Ok(BindingPattern::Object {
            properties: head,
            rest,
        })
    }

    pub fn parse_binding_property(&mut self) -> Result<BindingProperty> {
        let next = peek_expect!(self);
        match next.kind() {
            t!("string") => {
                self.next();
                let name = PropertyName::String(next.data_id().unwrap());
                let element = self.parse_binding_element()?;
                Ok(BindingProperty::Property { name, element })
            }
            t!("num") => {
                self.next();
                let name = PropertyName::Number(next.data_id().unwrap());
                let element = self.parse_binding_element()?;
                Ok(BindingProperty::Property { name, element })
            }
            t!("[") => {
                self.next();
                let expr = self.parse_assignment_expr()?;
                expect!(self, "]");
                let name = PropertyName::Computed(expr);
                let element = self.parse_binding_element()?;
                Ok(BindingProperty::Property { name, element })
            }
            _ => {
                let ident = self.parse_ident()?;
                if self.eat(t!(":")) {
                    let name = PropertyName::Ident(ident);
                    let element = self.parse_binding_element()?;
                    Ok(BindingProperty::Property { name, element })
                } else {
                    let initializer = self
                        .eat(t!("="))
                        .then(|| self.parse_assignment_expr())
                        .transpose()?;
                    Ok(BindingProperty::Binding {
                        name: next.data_id().unwrap(),
                        initializer,
                    })
                }
            }
        }
    }

    pub fn parse_array_pattern(&mut self) -> Result<BindingPattern> {
        let mut head = ListHead::Empty;
        let mut prev = None;
        let mut rest = None;
        loop {
            let next = peek_expect!(self, "]");
            match next.kind() {
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
                    let item = self.ast.push_node(None);
                    prev = Some(self.ast.append_list(item, prev));
                    head = head.or(prev.into());
                }
                _ => {
                    let elem = self.parse_binding_element()?;
                    let item = self.ast.push_node(Some(elem));
                    prev = Some(self.ast.append_list(item, prev));
                    head = head.or(prev.into());
                    if !self.eat(t!(",")) {
                        break;
                    }
                }
            }
        }
        expect!(self, "]");

        Ok(BindingPattern::Array {
            elements: head,
            rest,
        })
    }

    pub fn parse_binding_element(&mut self) -> Result<BindingElement> {
        let next = peek_expect!(self);
        match next.kind() {
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
                let name = self.parse_ident()?;
                let initializer = self
                    .eat(t!("="))
                    .then(|| self.parse_assignment_expr())
                    .transpose()?;
                Ok(BindingElement::SingleName { name, initializer })
            }
        }
    }
}
